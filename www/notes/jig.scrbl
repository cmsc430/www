#lang scribble/manual

@(require (for-label (except-in racket ... compile) a86))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86))
@(ev `(current-directory ,(path->string (build-path notes "jig"))))
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '("interp.rkt" "compile.rkt" "ast.rkt" "parse.rkt" "types.rkt"))

@(define this-lang "Jig")

@title[#:tag this-lang]{@|this-lang|: jumping to tail calls}

@src-code[this-lang]

@table-of-contents[]

@section[#:tag-prefix "jig"]{Tail Calls}

With Iniquity, we've finally introduced some computational power via
the mechanism of functions and function calls.  Together with the
notion of inductive data, which we have in the form of pairs, we can
write fixed-sized programs that operate over arbitrarily large data.

The problem, however, is that there are a class of programs that
should operate with a fixed amount of memory, but instead consume
memory in proportion to the size of the data they operate on.  This is
unfortunate because a design flaw in our compiler now leads to
asympototically bad run-times.

We can correct this problem by generating space-efficient code for
function calls when those calls are in @bold{tail position}.

Let's call this language @bold{Jig}.

There are no syntactic additions required: we simply will properly
handling function calls.


@section[#:tag-prefix "jig"]{What is a Tail Call?}

A @bold{tail call} is a function call that occurs in @bold{tail
position}.  What is tail position and why is important to consider
function calls made in this position?

Tail position captures the notion of ``the last subexpression that
needs to be computed.''  If the whole program is some expression
@racket[_e], the @racket[_e] is in tail position.  Computing
@racket[_e] is the last thing (it's the only thing!) the program needs
to compute.

Let's look at some examples to get a sense of the subexpressions in
tail position.  If @racket[_e] is in tail position and @racket[_e] is
of the form:

@itemlist[

@item{@racket[(let ((_x _e0)) _e1)]: then @racket[_e1] is in tail
position, while @racket[_e0] is not.  The reason @racket[_e0] is not
in tail position is because after evaluating it, the @racket[_e1]
still needs to be evaluated.  On the other hand, once @racket[_e0] is
evaluated, then whatever @racket[_e1] evaluates to is what the whole
@racket[let]-expression evaluates to; it is all that remains to
compute.}

@item{@racket[(if _e0 _e1 _e2)]: then @emph{both} @racket[_e1] and
@racket[_e2] are in tail position, while @racket[_e0] is not.  After
the @racket[_e0], then based on its result, either @racket[_e1] or
@racket[_e2] is evaluated, but whichever it is determines the result
of the @racket[if] expression.}

@item{@racket[(+ _e0 _e1)]: then neither @racket[_e0] or @racket[_e1]
are in tail position because after both are evaluated, their results
still must be added together.}

@item{@racket[(f _e0 ...)], where @racket[f] is a function
@racket[(define (f _x ...) _e)]: then none of the arguments
@racket[_e0 ...] are in tail position, because after evaluating them,
the function still needs to be applied, but the body of the function,
@racket[_e] is in tail position.}

]

The significance of tail position is relevant to the compilation of
calls.  Consider the compilation of a call as described in
@secref{Iniquity}: a return address is pushed on the stack,
arguments are pushed on the call stack, then the
@racket[Jmp] instruction is issued, which jumps to the called function.
The function computes its result, pops its arguments, the pops the return
address and jumps back to the caller.

But if the call is in tail position, what else is there to do?
Nothing.  So after the call, return transfers back to the caller, who
then just pops their arguments and returns to their caller.

This leads to unconditional stack space consumption on @emph{every}
function call, even function calls that don't need to consume space.

Consider this program:

@#reader scribble/comment-reader
(racketblock
;; (Listof Number) -> Number
(define (sum xs) (sum/acc xs 0))

;; (Listof Number) Number -> Number
(define (sum/acc xs a)
  (if (empty? xs)
      a
      (sum/acc (cdr xs) (+ (car xs) a))))
)

The @racket[sum/acc] function @emph{should} operate as efficiently as
a loop that iterates over the elements of a list accumulating their
sum.  But, as currently compiled, the function will push stack frames
for each call.

Matters become worse if we were re-write this program in a seemingly
benign way to locally bind a variable:

@#reader scribble/comment-reader
(racketblock
;; (Listof Number) Number -> Number
(define (sum/acc xs a)
  (if (empty? xs)
      a
      (let ((b (+ (car xs) a)))
        (sum/acc (cdr xs) b))))
)

Now the function pushes a return point @emph{and} a local binding for
@racket[b] on every recursive call.

But we know that whatever the recursive call produces is the answer to
the overall call to @racket[sum].  There's no need for a new return
point and there's no need to keep the local binding of @racket[b]
since there's no way this program can depend on it after the recursive
call.  Instead of pushing a new, useless, return point, we should make
the call with whatever the current return address is, because that's
where control is going to jump to anyway.  This is the idea of
@tt{proper tail calls}.

@bold{An axe to grind:} the notion of proper tail calls is often
referred to with misleading terminology such as @bold{tail call
optimization} or @bold{tail recursion}.  Optimization seems to imply
it is a nice, but optional strategy for implementing function calls.
Consequently, a large number of mainstream programming languages, most
notably Java, do not properly implement tail calls.  But a language
without proper tail calls is fundamentally @emph{broken}.  It means
that functions cannot reliably be designed to match the structure of
the data they operate on.  It means iteration cannot be expressed with
function calls.  There's really no justification for it.  It's just
broken.  Similarly, it's not about recursion alone (although it is
critical @emph{for} recursion), it really is about getting function
calls, all calls, right. @bold{/rant}



@section[#:tag-prefix "jig"]{An Interpreter for Proper Calls}

Before addressing the issue of compiling proper tail calls, let's
first think about the interpreter, starting from the interpreter we
wrote for Iniquity:

@codeblock-include["iniquity/interp.rkt"]

What needs to be done to make it implement proper tail calls?

Well... not much.  Notice how every Iniquity subexpression that is in
tail position is interpreted by a call to @racket[interp-env] that is
itself in tail position in the Racket program!

So long as Racket implements tail calls properly, which is does, then
this interpreter implements tail calls properly.  The interpreter
@emph{inherits} the property of proper tail calls from the
meta-language.  This is but one reason to do tail calls correctly.
Had we transliterated this program to Java, we'd be in trouble as the
interpeter would inherit the lack of tail calls and we would have to
re-write the interpreter, but as it is, we're already done.

@section[#:tag-prefix "jig"]{A Compiler with Proper Tail Calls}

Consider the following program:

@#reader scribble/comment-reader
(racketblock
(define (f x)
  (if (zero? x)
      42
      (f (sub1 x))))
(f 100)
)

It's a silly program, but it will help illuminate what tail calls are
all about and how we can make them work.

Here's what this code will compile to, roughly:

@(void (ev '(current-objs '())))

@#reader scribble/comment-reader
(ex
(asm-interp
 (seq (Label 'entry)
      
      ;; calling (f 100), so set up return address,
      ;; push argument, then jump
      (Lea 'rax 'r1)
      (Push 'rax)
      (Mov 'rax 100)
      (Push 'rax)
      (Jmp 'f)
      (Label 'r1)
      
      ;; done with (f 100), return
      (Ret)
      
      ;; (define (f x) ...)
      (Label 'f)
      (Mov 'rax (Offset 'rsp 0))
      (Cmp 'rax 0)
      (Jne 'if_false)
      
      ;; if-then branch
      (Mov 'rax 42)
      (Jmp 'done)
      
      ;; if-else branch
      (Label 'if_false)
      ;; calling (f (sub1 x)), so set up return address,
      ;; push argument, then jump
      (Lea 'rax 'r2)
      (Push 'rax)
      (Mov 'rax (Offset 'rsp 8))
      (Sub 'rax 1)
      (Push 'rax)
      (Jmp 'f)
      (Label 'r2)
      
      (Label 'done)
      (Add 'rsp 8)  ; pop x
      (Ret)))
)

Now let's think about how this computes, paying attention to the stack.

First, the run-time system would call @racket['entry], so there's
going to be an address on the stack telling us where to return to when
the program is done:

@verbatim|{
         + ---------------------+
rsp ---> |   return to runtime  |
         +----------------------+
}|

Next, the call to @racket[(f 100)] is set up, pushing the address of
@racket['r1] for where the call should return to, and then pushing the
argument, @racket[100].  So before the @racket[Jmp] to @racket['f],
the stack looks like:

@verbatim|{
         + ---------------------+
         |   return to runtime  |
         +----------------------+
         |   return to r1       |
         +----------------------+
rsp ---> |   x : 100            |
         +----------------------+
}|

Control jumps to @racket['f], which asks if @racket[x] is 0 by
referencing the argument on the top of the stack.  It is not, so
control jumps to the @racket['if_false] label, which now sets up the
call @racket[(f (sub1 x))] by computing a return address for
@racket['r2], pushing it, subtracting @racket[1] from @racket[x], and
pushing that, then jumping to @racket['f].

Now the stack looks like:

@verbatim|{
         + ---------------------+
         |   return to runtime  |
         +----------------------+
         |   return to r1       |
         +----------------------+
         |   x : 100            |
         +----------------------+
         |   return to r2       |
         +----------------------+
rsp ---> |   x : 99             |
         +----------------------+
}|

This asks if @racket[x] is 0 by referencing the argument on the top of
the stack (now: @racket[99]).  It is not, so control jumps to the
@racket['if_false] label, which now sets up the call @racket[(f
(sub1 x))] by computing a return address for @racket['r2], pushing it,
subtracting @racket[1] from @racket[x], and pushing that, then jumping
to @racket['f].

@verbatim|{
         + ---------------------+
         |   return to runtime  |
         +----------------------+
         |   return to r1       |
         +----------------------+
         |   x : 100            |
         +----------------------+
         |   return to r2       |
         +----------------------+
         |   x : 99             |
         +----------------------+
         |   return to r2       |
         +----------------------+
rsp ---> |   x : 98             |
         +----------------------+
}|

You can see where this is going.

@verbatim|{
         + ---------------------+
         |   return to runtime  |
         +----------------------+
         |   return to r1       |
         +----------------------+
         |   x : 100            |
         +----------------------+
         |   return to r2       |
         +----------------------+
         |   x : 99             |
         +----------------------+
         |   return to r2       |
         +----------------------+
         |   x : 98             |
         +----------------------+
	 |          .           |
	 |          .           |
         |          .           |
         +----------------------+	 
         |   return to r2       |
         +----------------------+
         |   x : 1              |
         +----------------------+	 
         |   return to r2       |
         +----------------------+
rsp ---> |   x : 0              |
         +----------------------+
}|

At this point, we make a final jump to @racket['f].  Since @racket[x]
is @racket[0], @racket[42] is moved into @racket['rax], control jumps
tp @racket['done], at which point we pop the current @racket[x] off
the stack, then return, which pops off the next frame of the stack.
Since that frame says to jump to @racket['r2], that's where control
jumps to.

But @racket['r2] is the same as @racket['done]!  So we pop off the
current @racket[x] (now: @racket[1]) and return, which pops of the
next frame saying jump to @racket['r2].

This process continues, popping two frames and jumping back to
@racket['r2] until the stack looks like:

@verbatim|{
         + ---------------------+
         |   return to runtime  |
         +----------------------+
         |   return to r1       |
         +----------------------+
rsp ---> |   x : 100            |
         +----------------------+
}|

And we're back at @racket['r2].  Next we pop the current @racket[x]
(now: 100) and return, which pops the top frame off and jumps to
@racket['r1].  But the code following @racket['r1] is simply a
@racket[Ret] intruction, so we pop another frame (the stack is now
empty) and jump back to the runtime system (with @racket['rax] holding
@racket[42]).

So to summarize, each call to @racket[f] pushes two words on the
stack: one for where to return and one for the argument to the
function.  Then, when the base case is finally reached, we
spin through a loop popping all this information off.

Let's take another look at the function:

@#reader scribble/comment-reader
(racketblock
(define (f x)
  (if (zero? x)
      42
      (f (sub1 x))))
)

In the call to @racket[(f (sub1 x))], that expression is in a tail
position of the function.  Intuitively this means once you've computed
the result of @racket[(f (sub1 x))] there's nothing further to
compute, you now have the answer for @racket[(f x)].  This suggests
that you don't need to keep the current binding for @racket[x] on the
stack; if there's no further work to do, you can't possibly need
@racket[x].  It also suggests there's no need to return to the point
after @racket[(f (sub1 x))]; you could instead just return to the
caller of @racket[(f x)]!

We can modify the code to embody these ideas:

@#reader scribble/comment-reader
(ex
(asm-interp
 (seq (Label 'entry)
      
      ;; calling (f 100), so set up return address,
      ;; push argument, then jump
      (Lea 'rax 'r1)
      (Push 'rax)
      (Mov 'rax 100)
      (Push 'rax)
      (Jmp 'f)
      (Label 'r1)
      
      ;; done with (f 100), return
      (Ret)
      
      ;; (define (f x) ...)
      (Label 'f)
      (Mov 'rax (Offset 'rsp 0))
      (Cmp 'rax 0)
      (Jne 'if_false)
      
      ;; if-then branch
      (Mov 'rax 42)
      (Jmp 'done)
      
      ;; if-else branch
      (Label 'if_false)
      ;; TAIL calling (f (sub1 x)),
      ;; so pop off the argument (don't need it anymore)
      ;; and don't push a new return address, just leave
      ;; our caller's return address on stack
      (Mov 'rax (Offset 'rsp 0))
      (Sub 'rax 1)
      (Add 'rsp 8) ; pop x
      (Push 'rax)  ; push arg
      (Jmp 'f)
      
      (Label 'done)
      (Add 'rsp 8)  ; pop x
      (Ret)))
)

Let's step through the computation again.  It starts off the same: the
runtime calls @racket['entry], which sets up the call to @racket[(f
100)], so when control jumps to @racket['f], the stack again looks
like:

@verbatim|{
         + ---------------------+
         |   return to runtime  |
         +----------------------+
         |   return to r1       |
         +----------------------+
rsp ---> |   x : 100            |
         +----------------------+
}|

The checks if @racket[x] is @racket[0], which it is not, so jumps to
@racket['if_false].  Now the code computes @racket[x-1] and then pops
@racket[x], and pushes @racket[x-1], so when we jump to @racket['f],
the stack looks like:

@verbatim|{
         + ---------------------+
         |   return to runtime  |
         +----------------------+
         |   return to r1       |
         +----------------------+
rsp ---> |   x : 99             |
         +----------------------+
}|

Again we go through the same instructions, popping @racket[x] and
pushing @racket[x-1], then jumping to @racket['f] with stack:

@verbatim|{
         + ---------------------+
         |   return to runtime  |
         +----------------------+
         |   return to r1       |
         +----------------------+
rsp ---> |   x : 98             |
         +----------------------+
}|

This continues, but the stack never grows further, until finally jumping to @racket['f]
with the stack:

@verbatim|{
         + ---------------------+
         |   return to runtime  |
         +----------------------+
         |   return to r1       |
         +----------------------+
rsp ---> |   x : 0              |
         +----------------------+
}|

At which point, @racket[42] is moved in to @racket['rax] and control
jumps to @racket['done], where @racket[x] is popped and then
@racket[Ret] pops the next frame and jumps to @racket['r1], which
issues another @racket[Ret], popping the stack (now empty) and jumping
back to the runtime system.

So this program makes 100 calls to @racket[f], but it uses a constant
amount of stack space.  Indeed, we could've made it @racket[(f 200)]
and it would still use the same three stack frames.  The program
is really computing a loop.

Moreover, we can do one better: notice that the initial call
@racket[(f 100)] is itself in a tail position: whatever it's result is
is the result of the whole program.  We can turn this in to a tail
call:

@#reader scribble/comment-reader
(ex
(asm-interp
 (seq (Label 'entry)

      ;; TAIL calling (f 100),
      ;; no args to pop
      ;; don't push a new return address, just leave
      ;; our caller's return address on stack
      (Mov 'rax 100)
      (Push 'rax)
      (Jmp 'f)

      ;; No need for this since we never come back:
      ;; (Ret)
      
      ;; (define (f x) ...)
      (Label 'f)
      (Mov 'rax (Offset 'rsp 0))
      (Cmp 'rax 0)
      (Jne 'if_false)
      
      ;; if-then branch
      (Mov 'rax 42)
      (Jmp 'done)
      
      ;; if-else branch
      (Label 'if_false)
      ;; TAIL calling (f (sub1 x)),
      ;; so pop off the argument (don't need it anymore)
      ;; and don't push a new return address, just leave
      ;; our caller's return address on stack
      (Mov 'rax (Offset 'rsp 0))
      (Sub 'rax 1)
      (Add 'rsp 8) ; pop x
      (Push 'rax)  ; push arg
      (Jmp 'f)
      
      (Label 'done)
      (Add 'rsp 8)  ; pop x
      (Ret)))
)

Now the stack looks like this:

@verbatim|{
         + ---------------------+
         |   return to runtime  |
         +----------------------+
rsp ---> |   x : 100            |
         +----------------------+
}|

decrementing until @racket[x] reaches @racket[0] at which point
@racket[42] is put in @racket['rax] and control jumps back to the
runtime system.


In general, when a function call @racket[(_f _e0 ...)]  is in tail
position, there are going to be some number of things currently pushed
on the stack, which are described by the current environment
@racket[_c].  To carry out a tail call, we need to pop all of those
things described by @racket[_c], then push the values of @racket[_e0
...] which are the arguments for @racket[_f], then jump to
@racket[_f].

There is a problem here, which is that we need to evaluate the
subexpressions @racket[_e0 ...] and doing so may depend on things in
the current environment, e.g. they may reference bound variables.

So we have to wait to pop the things described by @racket[_c] until
@emph{after} evaluating @racket[_e0 ...], but evaluating @racket[_e0
...] will need to save the values somewhere... and that somewhere is
the stack.

Let's say we have an expression that looks like this:

@#reader scribble/comment-reader
(racketblock
(let ((x 1))
  (let ((y 2))
    (f (+ x y) 5))))

The call to @racket[f] is in tail position and it will be compiled in
a compile-time environment of @racket['(y x)].  The compiler will need
to compile @racket[(+ x y)] in that same environment, but then emit code
to save the result on the stack while the next argument is evaluated.

That means by the time the arguments are evaluated and the call is
ready to be made, the stack will look like:

@verbatim|{
         + ---------------------+
         |   return address     |
         +----------------------+
         |   x : 1              |
         +----------------------+
         |   y : 2              |
         +----------------------+
         |       3              |
         +----------------------+
rsp ---> |       5              |
         +----------------------+	 
}|

At which point we need to remove the @racket[x] and @racket[y] part,
but then also have the arguments @racket[3] and @racket[5] sitting just
below the return address, i.e. we want:

@verbatim|{
         + ---------------------+
         |   return address     |
         +----------------------+
         |       3              |
         +----------------------+
rsp ---> |       5              |
         +----------------------+	 
}|

To accomplish, we rely on the following helper function for generating
code that moves arguments on the stack:

@#reader scribble/comment-reader
(racketblock
;; Integer Integer -> Asm
(define (move-args i off)
  (cond [(zero? off) (seq)]
        [(zero? i)   (seq)]
        [else
         (seq (Mov r8 (Offset rsp (* 8 (sub1 i))))
              (Mov (Offset rsp (* 8 (+ off (sub1 i)))) r8)
              (move-args (sub1 i) off))]))
)

It moves @racket[i] elements on the stack up @racket[off]-positions.
So if you have @racket[(length _c)] items in the environment and
@racket[_n] arguments, then @racket[(move-args _n (length _c))] will
move the arguments the right spot, just below the return address.

Once the arguments are moved to the proper spot on the stack, we can
pop off the local environment and jump.  So the complete code for
compiling a tail call is:

@#reader scribble/comment-reader
(racketblock
;; Id [Listof Expr] CEnv -> Asm
(define (compile-app-tail f es c)
  (seq (compile-es es c)
       (move-args (length es) (length c))
       (Add rsp (* 8 (length c)))
       (Jmp (symbol->label f))))
)

What's left is determining @emph{when} to use this strategy for a
function application and when to use the prior version which pushes a
return pointer.

The way we do this is to add a parameter to the expression compiler,
so the new signature is:

@#reader scribble/comment-reader
(racketblock
;; Expr CEnv Bool -> Asm
(define (compile-e e c t?)
   ...)
)

Calling @racket[(compile-e _e _c #t)] signals that the expression
@racket[_e] should be compiled assuming it is in tail position, while
@racket[(compile-e _e _c #f)] signals it is not in tail position.

If @racket[_e] is an application, then the compiler selects between
@racket[compile-app-nontail] and @racket[compile-app-tail] based on
@racket[t?].

If @racket[_e] is any other kind of expression that has
sub-expressions, then the compiler function for that form also adds a
@racket[t?] parameter and sets @racket[t?] to @racket[#f] for any that
are not tail positions and passes on the @racket[t?] given for those
in tail position.  For example, here is how @racket[begin] is
compiled:

@#reader scribble/comment-reader
(racketblock
;; Expr Expr CEnv Bool -> Asm
(define (compile-begin e1 e2 c t?)
  (seq (compile-e e1 c #f)
       (compile-e e2 c t?)))
)

There are two important places where @racket[t?] is seeded to @racket[#t]:

@itemlist[
@item{The top-level expression is in tail position.}
@item{The body of every function is in tail position.}
]

The complete compiler:

@codeblock-include["jig/compile.rkt"]
