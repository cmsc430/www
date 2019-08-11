#lang scribble/manual
@(require scribble/core racket/list)
@(require (for-label racket))
@(require redex/reduction-semantics
          redex/pict (only-in pict scale))

@(require scribble/examples racket/sandbox)

@(define core-racket
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator 'racket/base)))

@(core-racket '(require racket/match))

@(define-syntax-rule (ex e ...) (examples #:eval core-racket #:label #f e ...))



@(define-syntax-rule (render-grammar L)
   (scale (render-language L) 1))

@(define-syntax-rule (render-grammar/nts L nts)
   (scale (render-language L #:nts nts) 1))



@title[#:style 'unnumbered]{Racket}

@verbatim{
TODO:
- write style section
- give some more examples
- mention #lang racket
}


This section gives a concise overview of the subset of Racket we will
be using for this course.  As we see more features of Racket, this
document will be expanded to cover the new features.

@section{Getting Started}

Racket is available for all major operating systems from:

@centered{@link["https://racket-lang.org/"]{@tt{https://racket-lang.org/}}}

We will be using Racket 7.3, but any version from the past several
years should work fine.

There are two essential references:

@itemlist[
@item{@link["https://docs.racket-lang.org/guide/"]{The Racket Guide} - intended for those new to Racket, i.e. @emph{you!}}
@item{@link["https://docs.racket-lang.org/reference/"]{The Racket Reference} - the definitive, comprehensive manual for Racket}
]

Racket is a large, full-featured, batteries-included language
platform.  However, we will be using only a small subset of Racket.
This subset should be easy to pick up for anyone familiar with
functional programming.  If you're comfortable with basic OCaml,
Haskell, or even JavaScript, you shouldn't have much trouble learning
the Racket bits we will be using.

@section{IDE}

Racket comes with it's own IDE: DrRacket, which is the recommended way
to edit Racket files.  We will also be running Racket and its
associated tools from the command line.

If you'd like to use Emacs, there's a good
@link["https://www.racket-mode.com/"]{Racket mode}, but we recommend
using DrRacket for a while before switching to Emacs.  Using any other
editor is fine, too.

@section{Grammar}

A program is a sequence of definitions or expressions.

@(define unquote "whatever") @;{Needed to make redex happy with unquote in grammar}
@(define-language R0
  (d ::= (define x e) (define (x x ...) e))
  (e ::= (e e ...) (δ e ...) sv x (λ (x ...) e) (quasiquote qq) (match e [p e] ...))
  (qq ::= (qq ...) sv x (unquote e))
  (sv ::= b n s)
  (p ::= (quasiquote r) b n x s (cons p p))
  (r ::= b n x s (unquote p))
  (s ::= string)
  (b ::= #t #f)
  (n ::= integer)
  (x ::= variable)
  (δ ::= add1 sub1 = * + - list cons))

The grammar for the subset of Racket we will use is:

@(with-unquote-rewriter
  (lambda (lw) 
    (build-lw (list (build-lw "(" (lw-line lw) (lw-line-span lw) (lw-column lw) 1)
                    (build-lw 'unquote (lw-line lw) (lw-line-span lw) (+ 1 (lw-column lw)) 7)
                    (build-lw " " (lw-line lw) (lw-line-span lw) (+ 2 (lw-column lw)) 1)
                    (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (+ 3 (lw-column lw)) (lw-column-span lw))
                    (build-lw ")" (lw-line lw) (lw-line-span lw) (lw-column lw) 1))
               (lw-line lw)
               (lw-line-span lw)
               (lw-column lw)
               (+ 8 (lw-column-span lw))))

	       
  (render-grammar R0))

@section{Built-In Datatypes}

We will use:

@itemize[
@item{Booleans}
@item{Numbers}
@item{Strings}
@item{Symbols}
@item{Pairs and Lists}
]

We will make extensive use of @link["https://docs.racket-lang.org/guide/quote.html"]{@tt{quote}}.

@section{Definitions}

A definition takes the form:

@render-grammar/nts[R0 '(d)]

A definition @render-term[R0 (define x e)] defines @render-term[R0 x]
to stand for the value produced by evaluating @render-term[R0 e].

The @render-term[R0 (define (x_0 x_1 ...) e)] form is shorthand for
@render-term[R0 (define x_0 (λ (x_1 ...) e))].

@section{Style}

TODO: write style guidelines.

@section{Examples}

Here are some examples of writing various functions in our subset of Racket.

@#reader scribble/comment-reader
(ex

;; compute the product of a list of numbers
(define (prod xs)
  (match xs
    ['() 1]
    [(cons x xs) (* x (prod xs))]))

(prod '(1 2 3 4 5))

;; reverse a list
(define (rev xs)
  (rev/acc xs '()))

(define (rev/acc xs a)
  (match xs
    ['() a]
    [(cons x xs) (rev/acc xs (cons x a))]))

(rev '(1 2 3 4 5))
)



