#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require scribble/examples
          redex/pict
	  "../fancyverb.rkt"
	  "../utils.rkt"
	  "blackmail/semantics.rkt"
	  "utils.rkt"
	  "ev.rkt")

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "blackmail" f))))))
	   '("interp.rkt" "compile.rkt" "random.rkt" "asm/interp.rkt" "asm/printer.rkt"))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path notes "blackmail")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@(require (for-syntax "../utils.rkt" racket/base "utils.rkt"))
@(define-syntax (shell-expand stx)
   (syntax-case stx ()
     [(_ s ...)
      (parameterize ([current-directory (build-path notes "blackmail")])
        (begin (apply shell (syntax->datum #'(s ...)))
	       #'(void)))]))

@;{ Have to compile 42.s (at expand time) before listing it }
@(shell-expand "echo '#lang racket\n(add1 (add1 40))' > add1-add1-40.rkt" "racket -t compile-file.rkt -m add1-add1-40.rkt > add1-add1-40.s")

@title{Let's Do It Again!}

We've seen all the essential peices (a grammar, an AST data type
definition, an operational semantics, an interpreter, a compiler,
etc.) for implementing a programming language, albeit for an amazingly
simple language.

We will now, through a process of @bold{iterative refinement}, grow
the language to have an interesting set of features.

@section{Blackmail: incrementing and decrementing}

Our second language, which subsumes Abscond, is @bold{Blackmail}.
Expressions in Blackmail include integer literals and increment and
decrement operations.  It's still a dead simple language, but at least
programs @emph{do} something.

@section{Abstract syntax for Blackmail}

A Blackmail program consists of a single expression, and the grammar
of expressions is:

@centered{@render-language[B]}

So, @racket[0], @racket[120], and @racket[-42] are Blackmail programs,
but so are @racket['(add1 0)], @racket['(sub1 120)], @racket['(add1
(add1 (add1 -42)))].

A datatype for representing expressions can be defined as:

@#reader scribble/comment-reader
(racketblock
;; type Expr = 
;; | Integer
;; | `(add1 ,Expr)
;; | `(sub1 ,Expr)
)

A predicate for recognizing well-formed expressions is more involved
than Abscond, but still straightforward:

@codeblock-include["blackmail/syntax.rkt"]

@section{Meaning of Blackmail programs}

The meaning of a Blackmail program depends on the form of the expression:

@itemlist[
@item{the meaning of an integer literal is just the integer itself,}
@item{the meaning of an increment expression is one more than the meaning of its subexpression, and}
@item{the meaning of a decrement expression is one less than the meaning of its subexpression.}]

The operational semantics reflects this dependence on the form of the
expression by having three rules, one for each kind of expression:

@(define ((rewrite s) lws)
   (define lhs (list-ref lws 2))
   (define rhs (list-ref lws 3))
   (list "" lhs (string-append " " (symbol->string s) " ") rhs ""))

@(with-unquote-rewriter
   (lambda (lw)
     (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (lw-column lw) (lw-column-span lw)))
   (with-compound-rewriters (['+ (rewrite '+)]
                             ['- (rewrite '–)])
     (centered (begin (judgment-form-cases '(0)) (render-judgment-form 𝑩))
               (hspace 4)
               (begin (judgment-form-cases '(1)) (render-judgment-form 𝑩))
	       (hspace 4)
	       (begin (judgment-form-cases '(2)) (render-judgment-form 𝑩)))))

The first rule looks familiar; it's exactly the semantics of integers
from Abscond.  The second and third rule are more involved.  In
particular, they have @bold{premises} above the line.  If the premises
are true, the @bold{conclusion} below the line is true as well.  These
rules are @emph{conditional} on the premises being true.  This is in
contrast to the first rule, which applies unconditionally.

We can understand these rules as saying the following:
@itemlist[
@item{For all integers @math{i}, @math{(i,i)} is in @render-term[B 𝑩].}

@item{For expressions @math{e_0} and all integers @math{i_0} and
@math{i_1}, if @math{(e_0,i_0)} is in @render-term[B 𝑩] and @math{i_1
= i_0 + 1}, then @math{(@RACKET['(add1 (UNSYNTAX @math{e_0}))], i_1)}
is in @render-term[B 𝑩].}

@item{For expressions @math{e_0} and all integers @math{i_0} and
@math{i_1}, if @math{(e_0,i_0)} is in @render-term[B 𝑩] and @math{i_1
= i_0 - 1}, then @math{(@RACKET['(sub1 (UNSYNTAX @math{e_0}))], i_1)}
is in @render-term[B 𝑩].}
]

These rules are @bold{inductive}.  We start from the meaning of
integers and if we have the meaning of an expression, we can construct
the meaning of a larger expression.

This may seem a bit strange at the moment, but it helps to view the
semantics through its correspondence with an interpreter, which given
an expression @math{e}, computes an integer @math{i}, such that
@math{(e,i)} is in @render-term[B 𝑩].

Just as there are three rules, there will be three cases to the
interpreter, one for each form of expression:

@codeblock-include["blackmail/interp.rkt"]

@examples[#:eval ev
(blackmail-interp 42)
(blackmail-interp -7)
(blackmail-interp '(add1 42))
(blackmail-interp '(sub1 8))
(blackmail-interp '(add1 (add1 (add1 8))))
]

Here's how to connect the dots between the semantics and interpreter:
the interpreter is computing, for a given expression @math{e}, the
integer @math{i}, such that @math{(e,i)} is in @render-term[B 𝑩].  The
interpreter uses pattern matching to determine the form of the
expression, which determines which rule of the semantics applies.

@itemlist[

@item{if @math{e} is an integer @math{i}, then we're done: this is the
right-hand-side of the pair @math{(e,i)} in @render-term[B 𝑩].}

@item{if @math{e} is an expression @RACKET['(add1 (UNSYNTAX
@math{e_0}))], then we recursively use the interpreter to compute
@math{i_0} such that @math{(e_0,i_0)} is in @render-term[B 𝑩].  But
now we can compute the right-hand-side by adding 1 to @math{i_0}.}

@item{if @math{e} is an expression @RACKET['(sub1 (UNSYNTAX
@math{e_0}))], then we recursively use the interpreter to compute
@math{i_0} such that @math{(e_0,i_0)} is in @render-term[B 𝑩].  But
now we can compute the right-hand-side by substracting 1 from @math{i_0}.}

]

This explaination of the correspondence is essentially a proof (by
induction) of the interpreter's correctness:

@bold{Interpreter Correctness}: @emph{For all Blackmail expressions
@racket[e] and integers @racket[i], if (@racket[e],@racket[i]) in
@render-term[B 𝑩], then @racket[(blackmail-interp e)] equals
@racket[i].}

@section{An Example of Blackmail compilation}

Just as we did with Abscond, let's approach writing the compiler by
first writing an example.

Suppose we want to compile @racket['(add1 (add1 40))].  We already
know how to compile the @racket[40]: @racket['(mov rax 40)].  To do
the increment (and decrement) we need to know a bit more x86-64.  In
particular, the @tt{add} (and @tt{sub}) instruction is relevant.  It
increments the contents of a register by some given amount.

Concretely, the program that adds 1 twice to 40 looks like:

@filebox-include[fancy-nasm "blackmail/add1-add1-40.s"]

The runtime stays exactly the same as before.

@shellbox["make add1-add1-40.run" "./add1-add1-40.run"]

@section{A Compiler for Blackmail}

To represent these new instructions, we extend the Asm AST data type:

@filebox-include-fake[codeblock "blackmail/asm/ast.rkt"]{
#lang racket
;; type Instruction =
;; ...
;; | `(add ,Arg ,Arg)
;; | `(sub ,Arg ,Arg)
}

And correspondingly update the printer:

@filebox-include-fake[codeblock "blackmail/asm/printer.rkt"]{
#lang racket
;; Instruction -> String
(define (instr->string i)
  (match i
    ...
    [`(add ,a1 ,a2)
     (string-append "\tadd " (arg->string a1) ", " (arg->string a2) "\n")]
    [`(sub ,a1 ,a2)
     (string-append "\tsub " (arg->string a1) ", " (arg->string a2) "\n")]))
}

We can now print the assembly of our example from an AST:

@ex[
(asm-display
  '(entry (mov rax 40)
          (add rax 1)
          (add rax 1)
          ret))
]

The compiler consists of two functions: the first, which is given a
program, emits the entry point and return instructions, invoking
another function to compile the expression:

@codeblock-include["blackmail/compile.rkt"]

Notice that @racket[blackmail-compile-e] is defined by structural
recursion, much like the interpreter.


We can now try out a few examples:

@ex[
(blackmail-compile '(add1 (add1 40)))
(blackmail-compile '(sub1 8))
(blackmail-compile '(add1 (add1 (sub1 (add1 -8)))))
]

And give a command line wrapper for parsing, checking, and compiling
files:

@codeblock-include["blackmail/compile-file.rkt"]

Here it is in action:

@shellbox["echo \"#lang racket\\n(add1 (add1 9))\" > add1-add1-9.rkt"
          "racket -t compile-file.rkt -m add1-add1-9.rkt"]

And using the same @link["code/blackmail/Makefile"]{@tt{Makefile}}
setup as in Abscond, we capture the whole compilation process with a
single command:

@shellbox["make add1-add1-9.run" "./add1-add1-9.run"]

Likewise, to test the compiler from within Racket, we use the same
@link["code/blackmail/asm/interp.rkt"]{@tt{asm/interp.rkt}} code to
encapsulate running assembly code:

@ex[
(asm-interp (blackmail-compile '(add1 (add1 40))))
(asm-interp (blackmail-compile '(sub1 8)))
(asm-interp (blackmail-compile '(add1 (add1 (sub1 (add1 -8))))))
]

@section{Correctness and random testing}

We can state correctness similarly to how it was stated for Abscond:

@bold{Compiler Correctness}: @emph{For all expressions @racket[e] and
integers @racket[i], if (@racket[e],@racket[i]) in @render-term[B
𝑩], then @racket[(asm-interp (blackmail-compile e))] equals
@racket[i].}


And we can test this claim by comparing the results of running
compiled and interpreted programs, leading to the following property,
which hopefully holds:

@ex[
(define (check-compiler e)
  (check-eqv? (blackmail-interp e)
              (asm-interp (blackmail-compile e))))]

The problem, however, is that generating random Blackmail programs is
less obvious compared to generating random Abscond programs
(i.e. random integers).  Randomly generating programs for testing is
its own well studied and active research area.  To side-step this
wrinkle, we have provided a small utility for generating random
Blackmail programs (@link["code/blackmail/random.rkt"]{random.rkt}),
which you can use, without needing the understand how it was
implemented.

@ex[
(eval:alts (require "random.rkt") (void))
(random-expr)
(random-expr)
(random-expr)
(random-expr)
(random-expr)
(asm-display (blackmail-compile (random-expr)))
(for ([i (in-range 10)])
  (check-compiler (random-expr)))
]

@section{Looking back, looking forward}

We've now built two compilers; enough to start observing a pattern.

Recall the phases of a compiler described in
@secref["What does a Compiler look like?"].
Let's identify these peices in the two
compilers we've written:

@itemlist[
@item{@bold{Parsed} into a data structure called an @bold{Abstract Syntax Tree}

@itemlist[@item{we use @tt{read} to parse text into a s-expression}]}

@item{@bold{Checked} to make sure code is well-formed (and well-typed)

@itemlist[@item{we use a predicate, @racket[integer?] for Abscond and
@racket[expr?] for Blackmail, to check whether an s-expression is a
well-formed AST}]}

@item{@bold{Simplified} into some convenient @bold{Intermediate Representation}

@itemlist[@item{we don't do any; the AST is the IR}]}

@item{@bold{Optimized} into (equivalent) but faster program

@itemlist[@item{we don't do any}]}

@item{@bold{Generated} into assembly x86

@itemlist[@item{we use @racket[abscond-compile] and @racket[blackmail-compile] to generate assembly (in AST form),
  and use @racket[asm-display] to print concrete X86-64}]}

@item{@bold{Linked} against a run-time (usually written in C)

@itemlist[@item{we link against our run-time written in @tt{main.c}}]}

]

Our recipe for building compiler involves:

@itemlist[#:style 'ordered
@item{Build intuition with @bold{examples},}
@item{Model problem with @bold{data types},}
@item{Implement compiler via @bold{type-transforming-functions},}
@item{Validate compiler via @bold{tests}.}
]

As we move forward, the language we are compiling will grow.  As the
language grows, you should apply this recipe to grow the compiler
along with the language.
