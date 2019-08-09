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
- write about match
- write style section
- move OCaml to Racket into lecture notes, keep this minimal
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

@section{From OCaml to Racket}

@emph{Racket = OCaml with uniform syntax and no types (for now)}

Let's start by looking at something you know: OCaml.  In OCaml,
expressions can include literals for numbers, strings, booleans.  Here
we are using the OCaml read-eval-print-loop (REPL) to type in examples
and evaluate their results: 
@verbatim{
# 8;;
- : int = 8
# "ocaml";;
- : string = "ocaml"
# true;;
- : bool = true
# false;;
- : bool = false
}

Note that the @tt{;;} is not part of the expression syntax, but is a
terminator token, signalling to the REPL that the expression is
complete and ready to be evaluated.

The Racket REPL operates similarly, but doesn't require a terminator:

@ex[
8
"racket"
#t
#f
]

OCaml prints out the type of each expression, in addition to its
value, while Racket only prints the value.  The notation for booleans
is slightly different, but both languages agree on numbers, strings,
and booleans.  OCaml uses a @tt{#} prompt, while Racket uses @tt{>},
but these differences are immaterial.  The languages are essentially
the same so far.

OCaml uses an infix notation for writing operations.

@verbatim{
# 1 + 2 * 2;;
- : int = 5
}

The order of operations follows the usual mathematical precendence
rules (which you must memorize), or you can use parentheses to indicate grouping:

@verbatim{
# 1 + (2 * 2);;
- : int = 5
# (1 + 2) * 2;;
- : int = 6
}

Extraneous parenthesis are fine:

@verbatim{
# (((1))) + ((2 * 2));;
- : int = 5
}

Compared to many languages you may know, including OCaml, Racket
employs a uniform, minimalistic concrete syntax based on the concept
of parenthesized, prefix notation.

In this notation, parentheses play a much more central role.  They are
not optional and they signal the form of the expression.

Languages, like people, descend from their ancestors and inherit some
of their properties.  In the case of notation, Racket inherits the
Lisp (and Scheme) notation for programs.  It takes a bit of getting
used to, but once aclimated, the notation should feel lightweight and
consistent; there is verry little to memorize when it comes to syntax.

So in Racket, we would write:

@ex[
(+ 1 (* 2 2))
(* (+ 1 2) 2)
]

Note that there are no precendence rules for addition and
multiplication: the form of the expression makes it unambiguous.

Parenthesis indicate function applications, so adding extraneous
parens means something different than in OCaml:

@ex[
(eval:error (1)) ]

Here the parens are indicating a function application.  The
``function'' is the first subexpression within the parens,
i.e. @racket[1].  Of course, @racket[1] isn't a function and can't be
applied, hence the error.


OCaml also has a notation for writing functions:

@verbatim{
# fun x y -> x + y;;
- : int -> int -> int = <fun>
}

This make an anonymous function that consumes two integers and
produces their sum.

To apply it, we can write it justapoxed with arguments:

@verbatim{
# (fun x y -> x + y) 3 4;;
- : int = 7
}

Note that in OCaml, every function is a function of exactly one
argument.  Therefore @tt{fun x y -> x + y} is actuallty shorthand for
@tt{fun x -> fun y -> x + y}.

Applying such a function to fewer than 2 arguments will do a
@emph{partial} function application, which will produce a function
that take the remaining arguments:

@verbatim{
# (fun x y -> x + y) 3;;
- : int -> int = <fun>
}

To encode functions that must be given two arguments, a tuple can be
used:

@verbatim{
# fun (x, y) -> x + y;;
- : int * int -> int = <fun>
}

To apply such a function, it must be given a pair of integers:

@verbatim{
# (fun (x, y) -> x + y) (3, 4);;
- : int = 7
}

The use of @tt{(x, y)} here in the function parameters is actually a
@emph{pattern}.  This can be understood as shorthand for:

@verbatim{
# fun p -> match p with (x, y) -> x + y;;
- : int * int -> int = <fun>
}

So even this function is actually taking a single argument (which must
be a pair of numbers).

Racket has a similar notation for writing functions:

@ex[
(λ (x) (λ (y) (+ x y))) ; like (fun x -> fun y -> x + y)
]

To apply it, it must be written in parens, juxtaposed with arguments:

@ex[
(((λ (x) (λ (y) (+ x y))) 3) 4)
]




Functions in Racket do not always consume a single argument.  They can
consume 0, 1, or more arguments.

@ex[
(λ (x y) (+ x y))
]

This is not a shorthand for the function above it; rather it is a function that expects two arguments:

@ex[
((λ (x y) (+ x y)) 3 4)
]

Applying a function to the wrong number of arguments will result in an
error (and not perform partial function application):

@ex[
(eval:error ((λ (x y) (+ x y)) 3))
]

At the top-level in OCaml, variables can be defined with @tt{let} and
@tt{let rec}:

@verbatim{
# let x = 3;; 
val x : int = 3
# let y = 4;;
val y : int = 4
# x + y;;
- : int = 7
# let rec fact = fun n -> 
    match n with 
    | 0 -> 1
    | n -> n * (fact (n - 1));;
val fact : int -> int = <fun>
# fact 5;;
- : int = 120
}

In Racket, variables are defined with the @racket[define] form:

@ex[
(define x 3)
(define y 4)
(+ x y)

(define fact
  (λ (n)
   (match n
     [0 1]
     [n (* n (fact (- n 1)))])))

(fact 5)
]

In OCaml, function definitions can be written as:

@verbatim{
# let rec fact n = 
    match n with 
    | 0 -> 1 
    | n -> n * (fact (n - 1));;
val fact : int -> int = <fun>
}

This is just a shorthand for the definition written above in terms of
@tt{fun}.

Similarly in Racket, function definitions can be written as:

@ex[
(define (fact n)
  (match n
    [0 1]
    [n (* n (fact (- n 1)))]))
]

which is shorthand for the definition above using @racket[λ].

OCaml has a built-in list datatype.  The empty list is written @tt{[]}
and @tt{::} is an operation for ``consing'' an element on to a list.
So to build a list with three integer elements, 1, 2, and 3, you'd write:

@verbatim{
# 1 :: 2 :: 3 :: [];;
- : int list = [1; 2; 3]
}

The notation @tt{[1; 2; 3]} is just a shorthand for the above.

Racket has a built-in list datatype.  The empty list is written @racket['()]
and @racket[cons] is an operation for consing an element on to a list.
To build the same list, you'd write:
@ex[
(cons 1 (cons 2 (cons 3 '())))
]

The notation @racket[(list 1 2 3)] is shorthand for the above.


There is a slight difference here.  For one, OCaml lists must be @emph{homogeneous}.  You can have a list
of strings or a list of numbers, but you can't have a list of strings @emph{and} numbers.

@verbatim{
# ["a"; 3];;
Error: This expression has type int but an expression was expected of type
         string
}

In Racket, there is no such restriction:

@ex[
(list "a" 3)
]

Also, in Racket, @racket[cons] plays the role of both tupling (making
pairs) and making lists (making a pair of an element and another list).

So in OCaml, you could make a pair @tt{("a", 3)}.  In Racket, you'd
write @racket[(cons "a" 3)].  Note this is a pair and not a proper
list.  In OCaml, tuples and lists are disjoint things.  In Racket,
lists and tuples (pairs) are made out of the same stuff.

One of the built-in datatypes we will use often in Racket is that of a
@emph{symbol}.  A symbol is just an atomic piece of data.  The closest
cousin in OCaml is a @emph{polymorphic variant}.

For example, suppose we want to build up binary trees using
polymorphic variants.  (This is not the idiomatic OCaml programming,
which would instead use a plain old variant and datatype, but it
corresponds more closely to the style of Racket code we will write.)

@verbatim{
# `Leaf;;
- : [> `Leaf ] = `Leaf
# `Node (5, `Leaf, `Leaf);;
- : [> `Node of int * [> `Leaf ] * [> `Leaf ] ] = `Node (5, `Leaf, `Leaf)
}

We can define a datatype for binary trees:

@verbatim{
# type bt = [ `Leaf | `Node of int * bt * bt ];;
type bt = [ `Leaf | `Node of int * bt * bt ]
}

And functions operating on binary trees can be defined by pattern matching:

@verbatim{
# let rec sum bt = 
    match bt with
    | `Leaf -> 0
    | `Node (n, left, right) -> 
      n + sum left + sum right;;
val sum : ([< `Leaf | `Node of int * 'a * 'a ] as 'a) -> int = <fun>
}

To use the @tt{sum} function:
@verbatim{
# sum (`Node (5, `Node (7, `Leaf, `Leaf), `Leaf));;
- : int = 12
}

In Racket, we could write:
@ex[
'Leaf
'(Node 5 Leaf Leaf)
]

Although there's no data type definition @emph{mechanism}, we can use
the concept of a type definition to organize our program:

@ex[
(code:comment "A BT is `Leaf | `(Node ,Number ,BT ,BT)")
]

Functions operating on such trees could be defined as:

@ex[
(define (sum bt) (code:comment "BT -> Number")
  (match bt
   [`Leaf 0]
   [`(Node ,n ,left ,right) 
    (+ n (sum left) (sum right))]))

(sum `(Node 5 (Node 7 Leaf Leaf) Leaf))
]

@section{Grammar}

A program is a sequence of definitions or expressions.

@(define unquote "whatever") @;{Needed to make redex happy with unquote in grammar}
@(define-language R0
  (d ::= (define x e) (define (x x ...) e))
  (e ::= (e e ...) (δ e ...) sv x (λ (x ...) e) (quasiquote qq) (match e [p e] ...))
  (qq ::= (qq ...) sv x (unquote e))
  (sv ::= b n s)
  (p ::= (quasiquote r) b n x s)
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



