#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  "utils.rkt"
	  "ev.rkt"
	  "../fancyverb.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86))
@(ev `(current-directory ,(path->string (build-path langs "iniquity"))))
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '("interp.rkt" "compile.rkt" "ast.rkt" "parse.rkt" "types.rkt"))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path langs "iniquity")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@(define this-lang "Iniquity")

@title[#:tag this-lang]{@|this-lang|: function definitions and calls}

@src-code[this-lang]

@table-of-contents[]

@section[#:tag-prefix "iniquity"]{Functions}

With @secref{Hustle}, we removed a major computational
shortcoming by adding the ability to use inductively defined
data. Doing so gives programmers the ability to represent
arbitrarily large pieces of information.

And yet, the language remains hamstrung. It has no mechanism
to @emph{compute} with such data. Sure, a programmer could
compute the sum of the first @emph{n} elements of a list,
for some fixed @emph{n}. But the size of this program would
be proportional to the size of @emph{n}. Want to compute the
sum of a billion element list? You'll need (at least) a
billion expressions. Want to compute the sum of a larger
list? Write a longer program! But if you want to compute the
sum of @emph{any} list, regardless of its size? You'll need
an arbitrarily long program. Of course programs are always
of some fixed size, since after all, you have to write them
down and at some point you have to stop writing. This means
the expressiveness of our language is @emph{still} severely
restricted.

The solution is to bring in the computational analog of
inductive data. When you have arbitrarily large data, you
need arbitrarily long running computations to process them.
Crucially, these arbitrarily long running computations need
to be described by finite sized programs. The analog of
inductive data are @bold{recursive functions}.


So let's now remove the computational shackles by
incorporating @bold{ functions}, and in particular,
@bold{recursive functions}, which will allow us to compute over
arbitrarily large data with finite-sized programs.

Let's call it @bold{@|this-lang|}.

We will extend the syntax by introducing a new syntactic category of
@bold{programs}, which consist of a sequence of function definitions
followed by an expression:

@racketblock[
(define (_f0 _x00 ...) _e0)
(define (_f1 _x10 ...) _e1)
...
e
]

And the syntax of expressions will be extended to include function calls:

@racketblock[
(_fi _e0 ...)
]

where @racket[_fi] is one of the function names defined in the program.

Note that functions can have any number of parameters and,
symmetrically, calls can have any number of arguments.  A program
consists of zero or more function definitions followed by an
expression.


An example concrete @this-lang program is:

@codeblock-include["iniquity/example/len.rkt"]

To represent these kinds of programs, we extend the definition of ASTs
as follows:

@codeblock-include["iniquity/ast.rkt"]

The parser will need to be updated to parse programs, not just
expressions.  Since a program is a sequence of forms, we will assume
the reader will read in all of these forms and construct a list of the
elements.  So the program parser @racket[parse] takes a list
of s-expressions.  There is also a new parse for function definitions,
@racket[parse-definition].  The parser for expressions @racket[parse-e]
is updated to include function applications.

@codeblock-include["iniquity/parse.rkt"]

Because of the change from a program being a single expression to a
sequence, we have to update the utilities that read program files,
i.e. @tt{interp-stdin.rkt} and @tt{compile-stdin.rkt}:

@codeblock-include["iniquity/interp-stdin.rkt"]
@codeblock-include["iniquity/compile-stdin.rkt"]





@section[#:tag-prefix "iniquity"]{An Interpreter for Functions}

Writing an interpreter for @|this-lang| is not too hard.  The main idea is
that the interpretation of expression is now parameterized by a set of
function definitions from the program.  It serves as a second kind of
environment that gets passed around and is used to resolve function
definitions when interpreting function calls.

The way a function call is interpreted is to first interpret all of
the arguments, building up a list of results.  Then the definition of
the function being called is looked up.  If the function has the same
number of parameters as there are arguments in the call, the body of
the function is interpreted in an enviorment that maps each parameter
to to the corresponding argument.  That's it.

@codeblock-include["iniquity/interp.rkt"]

A couple of things to note:

@itemlist[

@item{since the function definition environment is passed along even
when interpreting the body of function definitions, this
interpretation supports recursion, and even mutual recursion.}

@item{functions are @emph{not} values (yet).  We cannot bind a
variable to a function.  We cannot make a list of functions.  We
cannot compute a function.  The first position of a function call is a
function @emph{name}, not an arbitrary expression.  Nevertheless, we
have significantly increased the expressivity of our language.}

]

We can try it out:

@ex[
(interp
 (parse
  '(define (double x) (+ x x))
  '(double 5)))
]

We can see it works with recursive functions, too. Here's a recursive
function for computing triangular numbers:

@ex[
(interp
  (parse
   '(define (tri x)
      (if (zero? x)
          0
          (+ x (tri (sub1 x)))))
     
   '(tri 9)))
 ]

We can even define mutually recursive functions such as @racket[even?]
and @racket[odd?]:

@ex[
(interp
  (parse
   '(define (even? x)
      (if (zero? x)
          #t
          (odd? (sub1 x))))
     
   '(define (odd? x)
      (if (zero? x)
          #f
          (even? (sub1 x))))
   '(even? 101)))]

And the utility for interpreting programs in files works as well:

@shellbox["cat example/len.rkt | racket -t interp-stdin.rkt -m"]


@section[#:tag-prefix "iniquity"]{Conventions of Calling}

We've seen how to make calls in assembly already and our compiler
emits code to call functions defined in C in our runtime such
@tt{write_byte} and @tt{read_byte}.  Let's review the basics.

Suppose we want a function that does the same thing as @racket[(define
(dbl x) (+ x x))].  We can implement it in assembly with a labelled
block of code:

@#reader scribble/comment-reader
(racketblock
(seq (Label 'dbl)
     (Mov 'rax (Offset 0 'rsp))
     (Add 'rax 'rax)
     (Ret))
)

This function expects its argument to be available as the first
position on the stack.  That's different from the calling convention
defined by the System V ABI and used to call C code, but we can make
our conventions for our language, so long as we're mindful of
respecting the System V ABI when interacting with code generated by
other compilers (e.g. @tt{gcc}).

So under a calling convention in which arguments are passed on the
stack, a caller should push a value for the argument before calling
the function and then pop it off after the function call returns:

@#reader scribble/comment-reader
(racketblock
(seq (%%% "Calling dbl(5)")
     (Mov 'rax 5)
     (Push 'rax)
     (Call 'dbl)
     ;; rax holds 10 now
     ;; pop the argument
     (Add rsp 8)))
     

This @emph{almost} works, but has a crucial flaw.  The problem is that
@racket[Call] is an instruction that pushes on the stack.  It pushes
the return address, i.e. the location of the instruction the function
should return to when it's done, which will be located in
@racket[(Offset 'rsp 0)] when control jumps to @racket[(Label 'dbl)].
That means that the argument will be in @racket[(Offset 'rsp 8)].

So we can touch-up the example as follows and it will work:

@(void (ev '(current-objs '())))

@#reader scribble/comment-reader
(ex
(asm-interp
  (seq (Global 'entry)
       (Label 'entry)
       (%%% "Calling dbl(5)")
       (Mov 'rax 5)
       (Push 'rax)
       (Call 'dbl)
       ;; rax holds 10 now
       ;; pop the argument
       (Add rsp 8) 
       (Ret)
       
       (Label 'dbl)
       (Mov 'rax (Offset 'rsp 8))
       (Add 'rax 'rax)
       (Ret)))       
)

One of the unfortunate things about this set-up is that the code for
@racket[dbl] has to ``skip'' past the return pointer on the stack to
access the arguments.

Think for a moment about a call in Racket:

@#reader scribble/comment-reader
(racketblock
(define (dbl x)
  (+ x x))

(dbl 5))

Once the function call has fully evaluated it's arguments (in this
case the argument is a literal, so it's already evaluated), then it
should evaluate the body of the called function in an environment in
which the parameter (here: @racket[x]) is bound to the argument
(@racket[5]), hence @racket[(dbl 5)] is equivalent to:

@#reader scribble/comment-reader
(racketblock
(let ((x 5))
  (+ x x))
)

The problem with this perspective on function calls is that it doesn't
work well with the @racket[Call] instruction pushing the return
pointer on as the top frame of the stack before jumping to the
function body.  In the @racket[let]-expression, @racket[x] occurs at
lexical address @racket[0], but because of the return address being on
the stack, the value of @racket[x] is really at @racket[(Offset 'rsp 8)].

We can fix this, but let's recall that @racket[Call] can be expressed
in terms of more primitive instructions: all a call is doing is
computing the return address---the location of the instruction
following the call---pushing that address on the stack, and then
jumping to the given label.

We can do this ourselves, although we will need to use a new
instruction: @racket[Lea]:

@#reader scribble/comment-reader
(racketblock
  (seq (%%% "Calling dbl(5)")
       (Mov 'rax 5)
       (Push 'rax)
       ;; Call 'dbl but without using Call
       (let ((rp (gensym)))
         (seq (Lea 'rax rp)
	      (Push 'rax)
              (Jmp 'dbl)
	      (Label rp)))
       ;; rax holds 10 now
       ;; pop the argument
       (Add rsp 8) 
       (Ret)))

The @racket[Lea] instruction is the ``load effective address''
instruction; it can compute the location of a given label.  Here we
are labelling the spot immediately after the jump to @racket[dbl],
which is where we'd like the function call to return to.

We can verify this works just like before:

@#reader scribble/comment-reader
(ex
(asm-interp
  (seq (Global 'entry)
       (Label 'entry)
       (%%% "Calling dbl(5)")
       (Mov 'rax 5)
       (Push 'rax)
       ;; Call but without using Call
       (let ((rp (gensym)))
         (seq (Lea 'rax rp)
	      (Push 'rax)
              (Jmp 'dbl)
	      (Label rp)))
       ;; rax holds 10 now
       ;; pop the argument
       (Add rsp 8) 
       (Ret)
       
       (Label 'dbl)
       (Mov 'rax (Offset 'rsp 8))
       (Add 'rax 'rax)
       (Ret)))       
)

What's nice about expressing things in their more primitive form is we
can now change the way in which calls are made.  For example, we can
now push the address on the stack @emph{before} the arguments:

@#reader scribble/comment-reader
(racketblock
  (seq (%%% "Calling dbl(5)")
       ;; Call 'dbl but without using Call
       (let ((rp (gensym)))
         (seq (Lea 'rax rp)
	      (Push 'rax)    ; push return address
              (Mov 'rax 5)
              (Push 'rax)    ; *then* push argument
              (Jmp 'dbl)
	      (Label rp)))
       ;; rax holds 10 now
       ;; pop the argument
       (Add rsp 8)
       (Ret)))

This way the called function can fetch variable bindings by their
lexical address, i.e. @racket[x] will be at @racket[(Offset rsp 0)].

The problem now is that the called function doesn't have the return
address at the top off the stack when it does its @racket[Ret], rather
it has the value of its argument.

But the function knows how many arguments it takes and these arguments
will be popped by the caller as soon as the function returns, so
here's an idea: let's have the called function pop the arguments off.
(Note that this is just like how @racket[let] works: it pops its
bindings off after the body is done.)  After the arguments are popped,
where is the return address on the stack?  @racket[(Offset 'rsp 0)].
So after the arguments are popped, @racket[(Ret)] works as expected.

Here's a complete version where the caller no longer pops the
arguments but instead leaves it up to the function:

@#reader scribble/comment-reader
(ex
(asm-interp
  (seq (Global 'entry)
       (Label 'entry)
       (%%% "Calling dbl(5)")
       ;; Call but without using Call
       (let ((rp (gensym)))
         (seq (Lea 'rax rp)
	      (Push 'rax)    ; push return address
              (Mov 'rax 5)
              (Push 'rax)    ; *then* push argument
              (Jmp 'dbl)
	      (Label rp)))
       ;; rax holds 10 now
       ;; no need to pop argument
       (Ret)
       
       (Label 'dbl)
       (Mov 'rax (Offset 'rsp 0)) ; x is at offset 0 now
       (Add 'rax 'rax)
       (Add 'rsp 8)               ; pop argument off
       (Ret)))
)

It works as expected.

Let's use this as the basis of our calling convention.

A function call should:

@itemize[
@item{Push a return address on the stack}
@item{Push all of the arguments on the stack}
@item{Jump to the function}]

The call will jump to the return address with all of these item popped
off the stack.

A function should:

@itemize[
@item{Access arguments on the stack, starting from offset @racket[0]}
@item{Before returning, pop all of the arguments on the stack}
@item{Then return}]

You may notice that things will go wrong if a call pushes a number of
arguments that doesn't match the number of parameters to the function,
e.g. compiling something like:

@#reader scribble/comment-reader
(racketblock
(define (dbl x)
  (+ x x))

(dbl 1 2 3)
)

In @this-lang, it's possible to statically determine whether the
function call's number of arguments match the function's number of
parameters and we can consider mismatches as syntax errors (and thus
our compiler need not worry about this happening).  In more expressive
languages, this won't be the case, but we can consider how to check
that these two numbers match at run-time.  For now, let's not worry
about it.

@section[#:tag-prefix "iniquity"]{Compiling Function Calls and Definitions}

With our calling convention in place, it's pretty easy to compile
function definitions and function calls.  A function definition:

@#reader scribble/comment-reader
(racketblock
(define (_f _x ...)
  _e))

Should be compiled as:

@#reader scribble/comment-reader
(racketblock
(seq (Label _f)
     (compile-e _e (list _x ...))
     (Add 'rsp (* 8 (length (list _x ....))))
     (Ret))
)

This creates a label based on the function's name. The body of the
function is compiled in an environment in which all of the parameters
are bound. After the body executes, all of the arguments are popped
from the stack, leaving the return address at the top of the stack,
at which point the function returns.

For a function call:

@#reader scribble/comment-reader
(racketblock
(_f _e0 ...)
)

We can uses the following helper for compiling a sequence of
expressions and pushing their values on the stack:

@#reader scribble/comment-reader
(racketblock
;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-es es (cons #f c)))]))
)

Using this, the call can be compiled as:

@#reader scribble/comment-reader
(racketblock
(let ((r (gensym 'ret)))
  (seq (Lea rax r)
       (Push rax)
       (compile-es es (cons #f c))
       (Jmp (symbol->label f))
       (Label r)))
)
       
Notice that we compile @racket[es] in a static environment that is one
frame larger than that of the call because we have pushed the return
address on the stack and need to adjust the offsets of variable
references in @racket[es].

It's convenient that we evaluate @racket[es], saving the results to
the stack, which is just where they need to be in order to make the
function call.  There is a subtle problem with this code though:
@racket[compile-es] generates code to execute the expression in
@racket[es] from left to right, pushing to the stack along the way.
Thus the last argument will be the first element of the stack and the
first argument will be the furthest element.  That suggests we should
compile the body of a function with its parameter list reversed so
that the last parameter is at offset @racket[0] and its first
parameter is as @racket[(sub1 _n)] where @racket[_n] is the number of
parameters.  Touching up the code, we compile function definitions as:

@#reader scribble/comment-reader
(racketblock
(seq (Label _f)
     (compile-e _e (reverse (list _x ...)))
     (Add 'rsp (* 8 (length (list _x ....))))
     (Ret))
)

Now writing the complete definitions for @racket[compile-define] and
@racket[compile-app], we have:

@#reader scribble/comment-reader
(racketblock
;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (seq (Label f)
          (compile-e e (reverse xs))
          (Add rsp (* 8 (length xs)))
          (Ret))]))

;; Id [Listof Expr] CEnv -> Asm
(define (compile-app f es c)
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         (compile-es es (cons #f c))
         (Jmp (symbol->label f))
         (Label r))))         
)



@section[#:tag-prefix "iniquity"]{On Names and Labels}

There is one final wrinkle, which is that identifiers in our language
include many things which are not valid labels for the Nasm assembler.
Hence compiling a function like:

@racketblock[
(define (^weird% x) x)
]

will cause the assembler to reject the emitted code since
@racket['^weird%] is not a valid label name.  Labels must consist only
of letters, numbers, _, $, ?, @"@", ~, and ?.

We solve this problem by using a function that maps arbitrary Racket
symbols to valid Nasm labels (represented as symbols).  The function
has the property distinct symbols always map to distinct labels.

@ex[(symbol->label '^weird%)]

Using this function, we can touch up our code:

@#reader scribble/comment-reader
(racketblock
;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (seq (Label (symbol->label f))
          (compile-e e (reverse xs))
          (Add rsp (* 8 (length xs)))
          (Ret))]))
)


@section[#:tag-prefix "iniquity"]{A Compiler for @|this-lang|}

The last piece of the puzzle is the function for emitting code for a
complete program:

@#reader scribble/comment-reader
(racketblock
;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)  
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '())
           (Ret)
           (compile-defines ds)
           (Label 'raise_error_align)
           (Sub rsp 8)
           (Jmp 'raise_error))]))
)

It relies on a helper @racket[compile-defines] for compiling each
function definition and flattening the assembly instructions into a
single list:

@#reader scribble/comment-reader
(racketblock
;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))
)


Here's an example of the code this compiler emits:

@ex[
(asm-display
 (compile
  (parse '(define (double x) (+ x x)) '(double 5))))
]

And we can confirm running the code produces results consistent with
the interpreter:

@ex[
(current-objs '("runtime.o"))
(define (run . p)
  (bits->value (asm-interp (compile (apply parse p)))))

(run '(define (double x) (+ x x))
     '(double 5))

(run '(define (tri x)
        (if (zero? x)
            0
            (+ x (tri (sub1 x)))))
     '(tri 9))

(run '(define (even? x)
        (if (zero? x)
            #t
            (odd? (sub1 x))))
     '(define (odd? x)
        (if (zero? x)
            #f
            (even? (sub1 x))))
     '(even? 101))
]

The complete compiler code:

@codeblock-include["iniquity/compile.rkt"]
