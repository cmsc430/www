#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86))
@(ev `(current-directory ,(path->string (build-path notes "iniquity"))))
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '("interp.rkt" "compile.rkt" "ast.rkt" "parse.rkt" "types.rkt"))

@title[#:tag "Iniquity"]{Iniquity: function definitions and calls}

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

Let's call it @bold{Iniquity}.

We will extend the syntax by introducing a new syntactic category of
programs, which have the shape:

@racketblock[
(begin
  (define (_f0 _x0 ...) _e0)
  (define (_f1 _x1 ...) _e1)
  ...
  _e)]

And the syntax of expressions will be extended to include function calls:

@racketblock[
(_fi _e0 ...)
]

where @racket[_fi] is one of the function names defined in the program.

Note that functions can have any number of parameters and,
symmetrically, calls can have any number of arguments.  A program
consists of zero or more function definitions followed by an
expression.


@section[#:tag-prefix "iniquity"]{An Interpreter for Functions}

Writing an interpreter for Inquity is not too hard.  The main idea is
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
(interp (parse '(begin (define (double x) (+ x x))
                       (double 5))))
]

We can see it works with recursive functions, too. Here's a recursive
function for computing triangular numbers:

@ex[
(interp (parse '(begin (define (tri x)
                         (if (zero? x)
                             0
                             (+ x (tri (sub1 x)))))
                       (tri 9))))
]

We can even define mutually recursive functions such as @racket[even?]
and @racket[odd?]:

@ex[
(interp (parse '(begin (define (even? x)
                         (if (zero? x)
                             #t
                             (odd? (sub1 x))))
                       (define (odd? x)
                         (if (zero? x)
                             #f
                             (even? (sub1 x))))
                       (even? 101))))
]

@section[#:tag-prefix "iniquity"]{Compiling a Call}

Turning to compilation, let's start small by supposing we have a
single, pre-defined function and we add to the language the ability to
call this function.

A function in assembly has an entry point (a label), followed by a
sequence of instruction, ending with the @tt{'ret} instruction.  As a
convention, we will pass all arguments to a function on the stack.

So here is @tt{Asm} representing a single function named @tt{double}

@racketblock[
(seq (Label 'double)
     (Mov 'rax (Offset 'rsp -1))
     (Add 'rax 'rax)
     (Ret))
]

This function takes one argument from the stack, adds it to itself,
leaving the result in @racket['rax] when it returns.

The @racket[Ret] instruction works in concert with the @racket[Call]
instruction, which can be given a label, designating which function to
call.

So if we wanted to call @racket[double] with an argument of 5, we'd
first need to write 5 in to the approrpriate spot in the stack, then
issue the @racket[(Call 'double)] instruction.

Since the @tt{double} code is reading from offset -1 from
@racket['rsp], it is tempting to assume this is where you should write
the argument:

@#reader scribble/comment-reader
(racketblock
(seq (Mov (Offset rsp -1) 5)
     (Call 'double)
     (Add 'rax 1)) ; rax now holds 11
)

The problem is here is that the @racket[Call] instruction works by
modifying the @racket['rsp] register.

Remember how @racket['rsp] points to an ``occupied'' memory location
and we said we just leave whatever is there alone?  We can now explain
what's going on.

The @racket[Call] instruction advances @racket['rsp] to the next word
of memory and writes the location of the instruction that occurs after
the @racket[Call] instruction.  This is a @bold{return pointer}.  It
then jumps to the beginning of the instruction sequence after the
label that is the argument of @racket[Call].  Those instruction
execute and when we get to @racket[Ret], the return instruction reads
that address stored in @racket[(Offset 'rsp 0)], moves @racket['rsp]
back one word, and jumps to the instruction pointed to by the return
pointer.

So calls and returns in assembly are really just shorthand for:
@itemlist[
@item{pushing an address (where to return) on the stack}
@item{jumping to a label}
@item{executing some code}
@item{poping the return point off the stack and jumping to it}
]

The problem with the function call we wrote above is that we put the
argument in @racket[(Offset 'rsp -1)], but then the @racket[Call]
advances (by decrementing) the @racket['rsp] register and writes the
return point in @racket[(Offset 'rsp 0)], but that's exactly where we
had put the argument!

The solution then, is to put the argument at index -2 from the
caller's perspective.  When the call is made, it will be at index -1
from the function's perspective:

@#reader scribble/comment-reader
(racketblock
(seq (Mov (Offset 'rsp -2) 5)
     (Call 'double)
     (Add 'rax 1)) ; rax now holds 11
)

Now that we have seen how to make a call and return in assembly, we
can tackle code generation for a function call @racket[(double _e)] in
our language.

@racketblock[
;; Expr CEnv -> Asm
(define (compile-call-double e0 c) 
  (seq (compile-e e0 c)
       (Mov (Offset 'rsp -2) 'rax) ; place result of e0 in stack
       (Call 'double)))
]

This will work if the program consists only of a call to
@racket[double], however it doesn't work in general.

To see the problem, notice how the call code always uses the index -2
for the first argument and index -1 will hold the return pointer when
the call is made.  But what if those spots are occuppied on the
stack!?  The problem is that we've always calculated stack offsets
statically and never mutated @racket['rsp].  But @racket[Call]
expects @racket['rsp] to be pointing to the top of the stack.

The solution is to emit code that will adjust @racket['rsp] to the top
of (our statically calculated) stack.  How much does @racket['rsp]
need to change?  It needs to be decremented by the number of items in
the static environment, @racket[c].  We can adjust @racket['rsp], make
the call, but after the call returns, we can adjust @racket['rsp] back
to where it was before the call.

The code is:

@#reader scribble/comment-reader
(racketblock
;; Expr CEnv -> Asm
(define (compile-call-double e0 c)
  (let ((h  (* 8 (length c))))
    (seq (compile-e e0 c)
         (Sub 'rsp h)
         (Mov (Offset 'rsp -2) rax) ; place result of e0 in stack
         (Call 'double)
         (Add 'rsp h))))
)

This makes calls work in any stack context.

It's easy to generalize this code to call any given function name:

@#reader scribble/comment-reader
(racketblock
;; Id Expr CEnv -> Asm
(define (compile-call f e0 c)
  (let ((h  (* 8 (length c))))
    (seq (compile-e e0 c)
         (Sub 'rsp h)
         (Mov (Offset 'rsp -2) 'rax)
         (Call f)
         (Add 'rsp h))))
)

If we want accept any number of arguments, we have to do a little more
work.

We rely on the following helpful function for compiling a list of
expressions and saving the results on the stack:


@#reader scribble/comment-reader
(racketblock
;; (Listof Expr) CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (Mov (Offset 'rsp (- (add1 (length c)))) 'rax)
          (compile-es es (cons #f c)))]))     
)

So to compile a call with any number of arguments:

@#reader scribble/comment-reader
(racketblock
;; Id (Listof Expr) CEnv -> Asm
(define (compile-call f es c)
  (let ((h  (* 8 (length c))))
    (seq (compile-es es (cons #f c))	
         (Sub 'rsp h)
         (Call f)
         (Add 'rsp h))))
)

Notice that we call @racket[compile-es] in an extended static
environment, that has one addition slot used.  This will bump the
location of all the argument results by one, leaving the first slot
available for the return pointer!

@section[#:tag-prefix "iniquity"]{Compiling a Function Definition}

Now that we can compile calls, we just need to be able to compile
function definitions such as:

@racketblock[
(define (double x)
  (+ x x))
]

The idea here is pretty simple.  The compiler needs to emit a label
for the function, such as @racket['double], followed by the
instructions for the body of the function.

The body of the function has a single free variable, @racket[x].  We
can compile the expression in a static environement @racket['(x)] so
that it resolves this variable to the first position on the stack,
which, thanks to the code we emit for calls, will hold the argument
value.

After the instructions for the body, a @racket[(Ret)] instruction is
emitted so that control transfers back to the caller.

So the code for compiling a function definition is:

@#reader scribble/comment-reader
(racketblock
;; Id Id Expr -> Asm
(define (compile-define f x e0)
  (seq (Label f)
       (compile-e e0 (list x))      
       (Ret)))
)

What about functions that take zero or more arguments?  That's easy,
just compile the body in an appropriate static environment.

@#reader scribble/comment-reader
(racketblock
;; Id (Listof Id) Expr -> Asm
(define (compile-define f xs e0)
  (seq (Label f)
       (compile-e e0 (reverse xs))
       (Ret)))
)

(Note that we reverse the parameter list due to the order in which
arguments are added to the stack.)


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
;; Id (Listof Expr) CEnv -> Asm
(define (compile-call f es c)
  (let ((h  (* 8 (length c))))
    (seq (compile-es es (cons #f c))
         (Sub 'rsp h)
         (Call (symbol->label f))
         (Add 'rsp h))))

;; Id (Listof Id) Expr -> Asm
(define (compile-define f xs e0)
  (seq (Label (symbol->label f))
       (compile-e e0 (reverse xs))
       (Ret)))
)


@section[#:tag-prefix "iniquity"]{A Compiler for Iniquity}

The last piece of the puzzle is the function for emitting code for a
complete program:

@#reader scribble/comment-reader
(racketblock
;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (seq (compile-entry e)
          (compile-defines ds))]))
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
(displayln
 (asm-string
  (compile
   (parse '(begin (define (double x) (+ x x)) (double 5))))))
]

And we can confirm running the code produces results consistent with
the interpreter:

@ex[
(current-objs '("runtime.o"))
(define (run e)
  (asm-interp (compile (parse e))))

(run '(begin (define (double x) (+ x x))
             (double 5)))

(run '(begin (define (tri x)
               (if (zero? x)
                   0
                   (+ x (tri (sub1 x)))))
             (tri 9)))

(run '(begin (define (even? x)
               (if (zero? x)
                   #t
                   (odd? (sub1 x))))
             (define (odd? x)
               (if (zero? x)
                   #f
                   (even? (sub1 x))))
             (even? 101)))
]


The complete compiler code:

@codeblock-include["iniquity/compile.rkt"]
