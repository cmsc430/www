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
@(ev `(current-directory ,(path->string (build-path notes "iniquity"))))
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '("interp.rkt" "compile.rkt" "ast.rkt" "parse.rkt" "types.rkt"))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path notes "iniquity")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@(define this-lang "Iniquity")

@title[#:tag this-lang]{@|this-lang|: function definitions and calls}

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
i.e. @tt{interp-file.rkt} and @tt{compile-file.rkt}:

@codeblock-include["iniquity/interp-file.rkt"]
@codeblock-include["iniquity/compile-file.rkt"]





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
(interp
 (parse
  '[(define (double x) (+ x x))
    (double 5) ]))
]

We can see it works with recursive functions, too. Here's a recursive
function for computing triangular numbers:

@ex[
(interp
  (parse
   '[(define (tri x)
       (if (zero? x)
           0
           (+ x (tri (sub1 x)))))
     
     (tri 9)]))
 ]

We can even define mutually recursive functions such as @racket[even?]
and @racket[odd?]:

@ex[
(interp
  (parse
   '[(define (even? x)
       (if (zero? x)
           #t
           (odd? (sub1 x))))
     
     (define (odd? x)
       (if (zero? x)
           #f
           (even? (sub1 x))))
     (even? 101)]))]

And the utility for interpreting programs in files works as well:

@shellbox["racket -t interp-file.rkt -m example/len.rkt"]


@section[#:tag-prefix "iniquity"]{Compiling a Call}

Turning to compilation, let's start small by supposing we have a
single, pre-defined function and we add to the language the ability to
call this function.

A function in assembly has an entry point (a label), followed by a sequence of
instruction, ending with the @tt{'ret} instruction. A very common approach for
passing arguments to a function, which we will use here, is to pass arguments
via the stack, this way you don't have to worry as much about which registers
may or may not be used (at the cost of performance).


@#reader scribble/comment-reader
(racketblock
(seq (Push rax)    ; argument is now at rsp
     (Call 'double)
     (Add 'rax 1)) ; rax now holds 11
)

So far, so good! Now we can look at what the code for the @tt{double} might
look like:

@racketblock[
(seq (Label 'double)
     (Mov (Offset 'rsp 0) 5)
     (Add 'rax 'rax)
     (Ret))
]

This function takes one argument from the stack, adds it to itself,
leaving the result in @racket['rax] when it returns.

The @racket[Ret] instruction works in concert with the @racket[Call]
instruction, which can be given a label, designating which function to
call.

So if we wanted to call @racket[double] with an argument of 5, we'd push 5 on
the stack, then issue the @racket[(Call 'double)] instruction.

The @tt{double} code is reading from offset 0 from @racket['rsp], @emph{seems}
to make sense, since we are pushing the argument right before executing the
@racket[Call] instruction.

The problem is here is that the @racket[Call] instruction works by
modifying the @racket['rsp] register!

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
argument in @racket[(Offset 'rsp 0)], but then the @racket[Call]
advances (by decrementing) the @racket['rsp] register and writes the
return point in @racket[(Offset 'rsp 0)], so now the relative offset
from @racket['rsp] would be 8, not 0!

The solution then, is to make sure that the function knows that
it's arguments have been ``shifted'' by one slot.

@racketblock[
(seq (Label 'double)
     (Mov (Offset 'rsp 8) 5)
     (Add 'rax 'rax)
     (Ret))
]

We're not out of the woods yet. What we've described above @emph{would} work,
on an idealized machine. However, the System V x86_64 calling convention adds
one more constraint on us: @tt{rsp} @emph{must} be aligned to 16-bytes when a
function call is performed. This requires us to do some calculating before we
can determine whether we need to pad that stack. This requires us to know how
many things are currently on our stack, luckily we already have an environment
on hand, which provides this information.  So for @racket[double], it would
look like the following:


@#reader scribble/comment-reader
(racketblock
(define (compile-double-call e c)

  ; determine whether stack is 16-byte aligned
  ; based on the number of things on the stack + our argument
  (if (even? (add1 (length c)))

      ; Stack will be 16-byte aligned:
      (seq (compile-es e c)        ; generate code for the argument
           (Call 'double)
           (Add rsp 8))            ; pop argument

      ; Stack will _not_ be 16-byte aligned
      ; We need to pad the stack
      (seq (Sub rsp 8)                  ; pad stack
           (compile-es es (cons #f c))  ; generate code for the argument
                                        ; taking the pad into account
           (Call 'double)
           (Add rsp 16))))              ; pop args and pad
)))

This will work if the program consists only of a call to
@racket[double], however it doesn't work in general.

It's easy to generalize this code to call any given function name:

@#reader scribble/comment-reader
(racketblock
(define (compile-double-call e c)

  ; determine whether stack is 16-byte aligned
  ; based on the number of things on the stack + our argument
  (if (even? (add1 (length c)))

      ; Stack will be 16-byte aligned:
      (seq (compile-es e c)        ; generate code for the argument
           (Call 'double)
           (Add rsp 8))            ; pop argument

      ; Stack will _not_ be 16-byte aligned
      ; We need to pad the stack
      (seq (Sub rsp 8)                  ; pad stack
           (compile-es es (cons #f c))  ; generate code for the argument
                                        ; taking the pad into account
           (Call 'double)
           (Add rsp 16))))              ; pop args and pad
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
          (Push rax)
          (compile-es es (cons #f c)))]))     
)

So to compile a call with any number of arguments:

@#reader scribble/comment-reader
(racketblock
(define (compile-app f es c)
  (if (even? (+ (length es) (length c)))
      (seq (compile-es es c)
           (Call f)
           (Add rsp (* 8 (length es))))            ; pop args
      (seq (Sub rsp 8)                             ; adjust stack
           (compile-es es (cons #f c))
           (Call f)
           (Add rsp (* 8 (add1 (length es)))))))   ; pop args and pad
)

Notice that in the `else' branch we call @racket[compile-es] in an extended
static environment, that has one addition slot used.  This will bump the
location of all the argument results by one, which is necessary for the 16-byte
alignment.

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

What about functions that take zero or more arguments?  That's easy,
just compile the body in an appropriate static environment.

@#reader scribble/comment-reader
(racketblock
;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (seq (Label f)
          (compile-e e (parity (cons #f (reverse xs))))
          (Ret))]))
)

(Note that we reverse the parameter list due to the order in which
arguments are added to the stack.)

The @racket[parity] function is there to manage alignment appropriately.
Because we know that the @racket[Call] instruction must be executed with
@racket['rsp] being 16-byte aligned, and that @racket[Call] @emph{pushes} the
return pointer on the stack, we have to make sure that the environment
accurately portrays the stack as @emph{not} 16-byte aligned at the beginning of
the function's code. To do this we add a dummy value to the @emph{end} of the
environment if it has an even number of items (even would imply that we are
16-byte aligned, when we know that we are not).

@#reader scribble/comment-reader
(racketblock
(define (parity c)
  (if (even? (length c))
      (append c (list #f))
      c))
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
;; Id (Listof Expr) CEnv -> Asm
(define (compile-app f es c)
  (if (even? (+ (length es) (length c)))
      (seq (compile-es es c)
           (Call (symbol->label f))
           (Add rsp (* 8 (length es))))            ; pop args
      (seq (Sub rsp 8)                             ; adjust stack
           (compile-es es (cons #f c))
           (Call (symbol->label f))
           (Add rsp (* 8 (add1 (length es)))))))   ; pop args and pad

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (seq (Label (symbol->label f))
          (compile-e e (parity (cons #f (reverse xs))))
          (Ret))]))
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
     (prog (Extern 'peek_byte)
           (Extern 'read_byte)
           (Extern 'write_byte)
           (Extern 'raise_error)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '(#f))
           (Mov rdx rbx) ; return heap pointer in second return register
           (Ret)
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
   (parse '[(define (double x) (+ x x)) (double 5)]))))
]

And we can confirm running the code produces results consistent with
the interpreter:

@ex[
(current-objs '("runtime.o"))
(define (run p)
  (asm-interp (compile (parse p))))

(run '[(define (double x) (+ x x))
       (double 5)])

(run '[(define (tri x)
         (if (zero? x)
             0
             (+ x (tri (sub1 x)))))
       (tri 9)])

(run '[(define (even? x)
         (if (zero? x)
             #t
             (odd? (sub1 x))))
       (define (odd? x)
         (if (zero? x)
             #f
             (even? (sub1 x))))
       (even? 101)])
]


The complete compiler code:

@codeblock-include["iniquity/compile.rkt"]
