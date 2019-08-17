#lang scribble/manual
@title[#:style 'unnumbered #:tag "week2"]{Week 2: The First Few Compilers}

@table-of-contents[]

@include-section["abscond.scrbl"]

@;{
@section{Let's Do It Again!}

We've seen all the essential peices (a grammar, an AST data type
definition, an operational semantics, an interpreter, a compiler,
etc.) for implementing a programming language, albeit for an amazingly
simple language.

We will now, through a process of @bold{iterative refinement}, grow
the language to have an interesting set of features.

@subsection{Blackmail: incrementing and decrementing}

Our second language, which subsumes Abscond, is @bold{Blackmail}.
Expressions in Blackmail include integer literals and increment and
decrement operations.  It's still a dead simple language, but at least
programs @emph{do} something.

@subsection{Abstract syntax for Blackmail}

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


@subsection{Meaning of Blackmail programs}

The meaning of a Blackmail program depends on the form of the expression:

@itemlist[
@item{the meaning of an integer literal is just the integer itself,}
@item{the meaning of an increment expression is one more than the meaning of its subexpression, and}
@item{the meaning of a decrement expression is one less than the meaning of its subexpression.}]

The operational semantics reflects this dependence on the form of the
expression by having three rules, one for each kind of expression:

@(require (for-syntax racket/match))
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

@filebox-include[codeblock "blackmail-interp.rkt" "notes/2/blackmail-interp.rkt"]

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

}


@;{
@section{Lets Write a Compiler!}

Our goal is to write a compiler which is a function:

@verbatim{
compiler :: SourceProgram -> TargetProgram
}

In 430 TargetProgram is going to be a binary executable.

@subsection{Lets write our first Compilers}

@tt{SourceProgram} will be a sequence of four @emph{tiny} “languages”

@itemlist[#:style 'ordered
@item{Numbers, e.g. @racket[7], @racket[12], @racket[42], ...}
@item{Numbers + Increment, e.g. @racket[(add1 7)], @racket[(add1 (add1 7))], ...}
@item{Numbers + Increment + Decrement, e.g. @racket[(add1 7)], @racket[(add1 (add1 12))], @racket[(sub1 (add1 42))], ...}
@item{Numbers + Increment + Decrement + Local Variables, e.g. 
@racketblock[
(let* ((x (add1 7))
       (y (add1 x)))
  (add1 y))]
}
]

@subsection{Recall: What does a Compiler @emph{look like}?}

@image{img/compiler-pipeline.png}
Compiler Pipeline

An input source program is converted to an executable binary in many stages:

@itemlist[
@item{@bold{Parsed} into a data structure called an @bold{Abstract Syntax Tree}}
@item{@bold{Checked} to make sure code is well-formed (and well-typed)}
@item{@bold{Simplified} into some convenient @bold{Intermediate Representation}}
@item{@bold{Optimized} into (equivalent) but faster program}
@item{@bold{Generated} into assembly x86}
@item{@bold{Linked} against a run-time (usually written in C)}
]

@subsection{Simplified Pipeline}

@bold{Goal}: Compile @emph{source} into @emph{executable} that, when
run, @bold{prints} the result of evaluating the source.

@bold{Approach}: Lets figure out how to write

@itemlist[#:style 'ordered
@item{A @bold{compiler} from the input @emph{string} into @emph{assembly},}
@item{A @bold{run-time} that will let us do the printing.}
]

@image{img/compiler-pipeline-1-2.png}
Simplified Compiler Pipeline with Runtime

Next, lets see how to do (1) and (2) using our sequence of languages.


@section{A Compiler for Numbers}

This is perhaps the simplest programming language: it consists solely
of number literals.

@subsection{The "Run-time"}

Lets work @emph{backwards} and start with the run-time.

Here’s what it looks like as a C program @tt{main.c}

@verbatim{
#include <stdio.h>

extern int our_code() asm("our_code_label");

int main(int argc, char** argv) {
  int result = our_code();
  printf("%d\n", result);
  return 0;
}
}




@verbatim{
section .text
global our_code_label
our_code_label:
  mov eax, 42
  ret
}


}