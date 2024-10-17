#lang scribble/manual

@(require (for-label (except-in racket compile ... struct?) a86))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  "utils.rkt"
	  "ev.rkt"
	  "../fancyverb.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path langs "outlaw")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@(require (for-syntax "../utils.rkt" racket/base "utils.rkt"))
@(define-syntax (shell-expand stx)
   (syntax-case stx ()
     [(_ s ...)
      (parameterize ([current-directory (build-path langs "outlaw")])
        (begin (apply shell (syntax->datum #'(s ...)))
	       #'(void)))]))

@;{ Have to generate a-whole.rkt before listing it below.}
@(shell-expand "racket -t combine.rkt -m a.rkt > a-whole.rkt")

@(ev '(require rackunit a86))
@(ev `(current-directory ,(path->string (build-path langs "outlaw"))))
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))
@(void (ev '(current-objs '("runtime.o"))))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '(#;"interp.rkt" "compile.rkt" "compile-expr.rkt" "compile-literals.rkt" "compile-datum.rkt" "utils.rkt" "ast.rkt" "parse.rkt" "types.rkt"))

@(define this-lang "Outlaw")

@title[#:tag this-lang]{@|this-lang|: self-hosting}

@src-code[this-lang]

@emph{The king is dead, long live the king!}

@table-of-contents[]

@section[#:tag-prefix "neerdowell"]{Bootstrapping the compiler}

Take stock for a moment of the various language features we've built
over the course of these notes and assignments: we've built a
high-level language with built-in data types like booleans, integers,
characters, pairs, lists, strings, symbols, vectors, boxes.  Users can
define functions, including recursive functions. Functions are
themselves values and can be constructed anonymously with
@racket[lambda].  We added basic I/O facilities.  We added the ability
to overload functions based on the number of arguments received using
@racket[case-lambda], the ability to define variable arity functions
using rest arguments, and the ability to call functions with arguments
from a list using @racket[apply].  Users can defined their own
structure types and use pattern matching to destructure values.
Memory management is done automatically by the run-time system.

It's a pretty full-featured language and there are lots of interesting
programs we could write in our language.  One of the programs we could
@emph{almost} write is actually the compiler itself.  In this section,
let's bridge the gap between the features of Racket our compiler uses
and those that our compiler implements and then explore some of the
consequences.


We'll call it @bold{Outlaw}.

@section[#:tag-prefix "outlaw"]{Features used by the Compiler}

Let's take a moment to consider all of the language features we
@emph{use} in our compiler source code, but we haven't yet
implemented.  Open up the source code for, e.g. @secref{Neerdowell},
and see what you notice:

@itemlist[

@item{Modules: programs are not monolithic; they are broken into
@bold{modules} in separate files like @tt{compile-stdin.rkt},
@tt{parse.rkt}, @tt{compile.rkt}, etc.}

@item{a86: our compiler relies heavily on the @secref{a86} library
that provides all of the constructors for a86 instructions and
functions like @racket[asm-display] for printing a86 instructions
using NASM syntax.}

@item{Higher-level I/O: at the heart of the front-end of our compiler
is the use of Racket's @racket[read] function, which reads in an
s-expression.  We also use things like @racket[read-line] which reads
in a line of text and returns it as a string.}

@item{Lots and lots of Racket functions: our compiler makes use of
lots of built-in Racket functions that we haven't implemented.  These
are things like @racket[length], @racket[map], @racket[foldr],
@racket[filter], etc.  Even some of the functions we have implemented
have more featureful counterparts in Racket which we use.  For
example, our @racket[+] primitve takes two arguments, while Racket's
@racket[+] function can take any number of arguments.}

@item{Primitives as functions: the previous item brings up an
important distinction between our language and Racket.  For us,
things like @racket[+] are @bold{primitives}.  Primitives are
@emph{not} values.  You can't return a primitive from a function.  You
can't make a list of primitives.  This means even if we had a
@racket[map] function, you couldn't pass @racket[add1] as an argument,
since @racket[add1] is not a value.  In Racket, there's really no such
thing as a primitive; things like @racket[add1], @racket[+],
@racket[cons?], etc. are all just functions.}

]

If we want our compiler to be written in the language it implements we
have to deal with this gap in some way.  For each difference between
what we implement and what we use, we basically only have two ways to
proceed:

@itemlist[#:style 'ordered

  @item{rewrite our compiler source code to @emph{not} use
that feature, or}

  @item{implement it.}
]

Let's take some of these in turn.

@section[#:tag-prefix "outlaw"]{Punting on Modules}

Our compiler currently works by compiling a whole program, which we
assume is given all at once as input to the compiler.  The compiler
source code, on the other hand, is sensibly broken into seperate
modules.

We @emph{could} think about designing a module system for our
language.  We'd have to think about how seperate compilation of
modules would work.  At a minimum our compiler would have to deal with
resolving module references made through @racket[require].

While module systems are a fascinating and worthy topic of study, we
don't really have the time to do them justice and instead we'll opt to
punt on the module system.  Instead we can rewrite the compiler source
code as a single monolithic source file.

That's not a very good software engineering practice and it will be a
bit of pain to maintain the complete @this-lang source file.  As a
slight improvement, we can write a little utility program that given a
file containing a module will recursively follow all @racket[require]d
files and print out a single, @racket[require]-free program that
includes all of the modules that comprise the program.

Let's see an example of the @tt{combine.rkt} utility in action.

Suppose we have a program that consists of the following files:

@codeblock-include["outlaw/a.rkt"]
@codeblock-include["outlaw/b.rkt"]
@codeblock-include["outlaw/c.rkt"]

Then we can combine these files into a single program
as follows:

@shellbox["racket -t combine.rkt -m a.rkt > a-whole.rkt"]

@codeblock-include["outlaw/a-whole.rkt"]


This gives us a rudimentary way of combining modules into a single
program that can be compiled with our compiler.  The idea will be that
we construct a single source file for our compiler by running
@tt{combine.rkt} on @tt{compile-stdin.rkt}.  The resulting file will
be self-contained and include everything @tt{compile-stdin.rkt}
depends upon.

It's worth recognizing that this isn't a realistic alternative to
having a module system.  In particular, combining modules in this way
breaks usual abstractions provided by modules.  For example, it's
common for modules to define their own helper functions or stateful
data that are not exported (via @racket[provide]) outside the module.
This ensures that clients of the module cannot access potentially
sensitive data or operations or mess with invariants maintained by a
module's exports.  Our crude combination tool does nothing to enforce
these abstraction barriers.

That's an OK compromise to make for now.  The idea is that
@tt{combine.rkt} doesn't have to work @emph{in general} for combining
programs in a meaning-preserving way.  It just needs to work for one
specific program: our compiler.

@section[#:tag-prefix "outlaw"]{Bare-bones a86}

Our compiler makes heavy use of the @secref{a86} library that provides
all of the constructors for a86 instructions and functions like
@racket[asm-display] for printing a86 instructions using NASM syntax.
That library is part of the @tt{langs} package.

The library at its core provides structures for representing a86
instructions and some operations that work on instructions.  While the
library has a bunch of functionality that provides for good, early
error checking when you construct an instruction or a whole a86
program, we really only need the structures and functions of the
library.

To make the compiler self-contained we can build our own bare-bones
version of the a86 library and include it in the compiler.

For example, here's the module that defines an AST for a86 instructions:

@codeblock-include["outlaw/a86/ast.rkt"]

And here's the module that implements the needed operations for
writing out instructions in NASM syntax:

@codeblock-include["outlaw/a86/printer.rkt"]

OK, so now we've made a86 a self-contained part of the the compiler.
The code consists of a large AST definition and some functions that
operate on the a86 AST data type. The printer makes use of some Racket
functions we haven't used before, like @racket[system-type] and
@racket[number->string], and also some other high-level IO functions
like @racket[write-string].  We'll have to deal with these features,
so while we crossed one item of our list (a86), we added a few more,
hopefully smaller problems to solve.

@section[#:tag-prefix "outlaw"]{Racket functions, more I/O, and primitives}

We identified three more gaps between our compiler's implementation
language and its implemented language: lots of Racket functions like
@racket[length], @racket[map], etc., more I/O functions that operate
at a higher-level than our @racket[write-byte] and @racket[read-byte]
such as @racket[write-string], @racket[read], @racket[read-line],
etc., and finally the issue that primitives are not values.

There are many ways we could proceed from here.  We could, for
example, spend some time adding new primitives to our compiler
that implement all the missing functionality like @racket[length],
@racket[write-string], and others.

Let's consider adding a @racket[length] primitive.  It's not terribly
difficult.  We could add a unary operation called @racket['length],
which would emit the following code:

@#reader scribble/comment-reader
(racketblock
;; assume list is in rax
(let ((done (gensym 'done))
      (loop (gensym 'loop)))
  (seq (Mov r8 0)                ; count = 0
       (Label loop)
       (Cmp rax (value->bits '())) ; if empty, done
       (Je done)
       (assert-cons rax)         ; otherwise, should be a cons
       (Xor rax type-cons)
       (Mov rax (Offset rax 0))  ; move cdr into rax
       (Add r8 (value->bits 1))    ; increment count
       (Jmp loop)                ; loop
       (Label done)
       (Mov rax r8)))            ; return count
)

We can play around an make sure this assembly code is actually
computing the length of the list in @racket['rax]:

@(void (ev '(current-objs '())))

@#reader scribble/comment-reader
(ex
(require neerdowell/parse
         neerdowell/compile-datum
         neerdowell/compile-ops
         neerdowell/types)
(require a86)

;; Datum -> Natural
;; Computes the length of d in assembly
(define (length/asm d)
  (bits->value
   (asm-interp
    (seq (Global 'entry)
         (Label 'entry)
         (compile-datum d)
	 ; assume list is in rax
         (let ((done (gensym 'done))
               (loop (gensym 'loop)))
           (seq (Mov r8 0)                ; count = 0
                (Label loop)
                (Cmp rax (value->bits '())) ; if empty, done
                (Je done)
                (assert-cons rax)         ; otherwise, should be a cons
                (Xor rax type-cons)
                (Mov rax (Offset rax 0))  ; move cdr into rax
                (Add r8 (value->bits 1))    ; increment count
                (Jmp loop)                ; loop
                (Label done)
                (Mov rax r8)))            ; return count
         (Ret)
         (Label 'raise_error_align) ; dummy version, returns -1
         (Mov rax -1)
         (Ret)))))

(length/asm '())
(length/asm '(1 2 3))
(length/asm '(1 2 3 4 5 6))
)

Looks good.

Alternatively, instead of a primitive, we could add a @racket[length]
@emph{function} by creating a static function value and binding it to
the variable @racket[length].  The code for the function would
essentially be the same as the primitive above:

@#reader scribble/comment-reader
(racketblock
(seq (Data)
     (Label 'length_func) ; the length closure
     (Dq 'length_code)    ; points to the length code
     (Text)
     (Label 'length_code) ; code for length
     (Cmp r15 1) ; expects 1 arg
     (Jne 'raise_error_align)
     (Pop rax)
     ; ... length code from above
     (Add rsp 8) ; pop off function
     (Ret))
)


The @racket[compile] function could push the binding for
@racket[length] (and potentially other built-in functions) on the
stack before executing the instructions of the program compiled in an
environment that included @racket['length]. This would effectively
solve the problem for @racket[length].

We'd have to do something similar for @racket[map], @racket[foldr],
@racket[memq], and everything else we needed.


The @emph{problem} with this approach is will be spending a bunch of
time writing lots and lots of assembly code.  An activity we had hoped
to avoid by building a high-level programming language!  Even worse,
some of the functions we'd like to add, e.g. @racket[map], will be
much more complicated to write in assembly compared to @racket[length].

But here's the thing.  Consider a Racket definition of @racket[length]:

@#reader scribble/comment-reader
(racketblock
(define (length xs)
  (match xs
    ['() 0]
    [(cons _ xs) (add1 (length xs))]))
)

Note that this definition is within the language we've built.  Instead
of writing the assembly code for @racket[length], we could write a
definition in @this-lang and simply compile it to obtain assembly code
that implements a @racket[length] function.

Many of the functions we need in the compiler can be built up this
way.  Instead of spending our time writing and debugging assembly
code, which is difficulty to do, we can simply write some Racket code.

With this, we will introduce a @bold{standard library}.  The idea is that
the standard library, like the run-time system, is a bundle of code that
will accompany every executable; it will provide a set of built-in functions
and the compiler will be updated to compile programs in the environment of
everything provided by the standard library.


@section[#:tag-prefix "outlaw"]{Building a standard library}

...

@section[#:tag-prefix "outlaw"]{Parsing primitives, revisited}

...

@section[#:tag-prefix "outlaw"]{A few more primitives}

...

@section[#:tag-prefix "outlaw"]{Dealing with I/O}

...

@section[#:tag-prefix "outlaw"]{Putting it all together}

...
