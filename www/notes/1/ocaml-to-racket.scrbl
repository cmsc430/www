#lang scribble/manual
@(require "../../fancyverb.rkt" "../utils.rkt" "../../utils.rkt")
@(require scribble/core racket/list)
@(require (for-label racket rackunit))
@(require redex/reduction-semantics
          redex/pict (only-in pict scale))

@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(define core-racket
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator 'racket)))

@(core-racket '(require racket/match))

@(define-syntax-rule (ex e ...)
  (filebox (emph "Racket REPL")
    (examples #:eval core-racket #:label #f e ...)))

@(define (ocaml-repl . s)
  (filebox (emph "OCaml REPL")
    (apply fancyverbatim "ocaml" s)))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path notes "intro")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))


@title[#:tag "OCaml to Racket"]{From OCaml to Racket}

@emph{Racket = OCaml with uniform syntax and no types (for now)}

@table-of-contents[]

@section{Basic values}

Let's start by looking at something you know: OCaml.  In OCaml,
expressions can include literals for numbers, strings, booleans.  Here
we are using the OCaml read-eval-print-loop (REPL) to type in examples
and evaluate their results:

@ocaml-repl{
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


@section{Basic operations}

OCaml uses an infix notation for writing operations.

@ocaml-repl{
# 1 + 2 * 2;;
- : int = 5
}

The order of operations follows the usual mathematical precendence
rules (which you must memorize), or you can use parentheses to indicate grouping:

@ocaml-repl{
# 1 + (2 * 2);;
- : int = 5
# (1 + 2) * 2;;
- : int = 6
}

Extraneous parenthesis are fine:

@ocaml-repl{
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


@section{Functions}

OCaml also has a notation for writing functions:

@ocaml-repl{
# fun x y -> x + y;;
- : int -> int -> int = <fun>
}

This make an anonymous function that consumes two integers and
produces their sum.

To apply it, we can write it justapoxed with arguments:

@ocaml-repl{
# (fun x y -> x + y) 3 4;;
- : int = 7
}

Note that in OCaml, every function is a function of exactly one
argument.  Therefore @tt{fun x y -> x + y} is actuallty shorthand for
@tt{fun x -> fun y -> x + y}.

Applying such a function to fewer than 2 arguments will do a
@emph{partial} function application, which will produce a function
that take the remaining arguments:

@ocaml-repl{
# (fun x y -> x + y) 3;;
- : int -> int = <fun>
}

To encode functions that must be given two arguments, a tuple can be
used:

@ocaml-repl{
# fun (x, y) -> x + y;;
- : int * int -> int = <fun>
}

To apply such a function, it must be given a pair of integers:

@ocaml-repl{
# (fun (x, y) -> x + y) (3, 4);;
- : int = 7
}

The use of @tt{(x, y)} here in the function parameters is actually a
@emph{pattern}.  This can be understood as shorthand for:

@ocaml-repl{
# fun p -> match p with (x, y) -> x + y;;
- : int * int -> int = <fun>
}

So even this function is actually taking a single argument (which must
be a pair of numbers).

Racket has a similar notation for writing functions:

@ex[
(λ (x) (λ (y) (+ x y))) ; like (fun x -> fun y -> x + y)
]

You can also write this without the fancy @racket[λ] by
spelling it @racket[lambda]:

@ex[
(lambda (x) (lambda (y) (+ x y))) ; like (fun x -> fun y -> x + y)
]

(In DrRacket, to insert a ``λ'' press Cmd+\.)


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

@section[#:tag-prefix "ocaml"]{Definitions}

At the top-level in OCaml, variables can be defined with @tt{let} and
@tt{let rec}:

@ocaml-repl{
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

(Note that the use of square brackets here is stylistic:
from Racket's point of view as long as ``parentheses'' (e.g.
@tt|{({[}|) match, any kind is acceptable.)

In OCaml, function definitions can be written as:

@ocaml-repl{
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

Notice both OCaml and Racket have pattern matching forms, which are
quite useful for writing function in terms of a number of "cases."
More on this in a minute.

@section{Lists}

OCaml has a built-in list datatype.  The empty list is written @tt{[]}
and @tt{::} is an operation for ``consing'' an element on to a list.
So to build a list with three integer elements, 1, 2, and 3, you'd write:

@ocaml-repl{
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

@ocaml-repl{
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

This can be confusing the first time you encounter it, so
let's go over it a bit more.

In Racket (or any Lisp), @racket[cons] plays the role of
both the pair constructor and the list constructor.
Non-empty lists are a subset of pairs: they are pairs whose
second component is a list (either the empty list or another
pair whose second component is a list, etc.).

You can make pairs out of any kind of element and you can
make lists out of any kind of elements. We can precisely
define these sets as:

@#reader scribble/comment-reader
(ex
;; type ListofAny =
;; | '()
;; | (cons Any ListofAny)

;; type PairofAny =
;; | (cons Any Any)
)

Or, to give more useful parameterized definitions:

@#reader scribble/comment-reader
(ex
;; type (Listof A) =
;; | '()
;; | (cons A (Listof A))

;; type (Pairof A B) =
;; | (cons A B)
)

The functions @racket[first] and @racket[rest] operate on
non-empty @emph{lists}, producing the first element of the
list and the tail of the list, respectively.

@ex[
(first (cons 3 (cons 4 '())))
(rest (cons 3 (cons 4 '())))]

These function will produce errors if given something that
is a pair but not a list:

@ex[
(eval:error (first (cons 3 4)))
(eval:error (rest (cons 3 4)))]

On the other hand, the functions @racket[car] and
@racket[cdr] access the left and right components of a pair
(the names are admittedly awful and an artifact of Lisp
history):

@ex[
(car (cons 3 4))
(cdr (cons 3 4))]

When given pairs that are also lists, they behave just like
@racket[first] and @racket[rest]:

@ex[
(car (cons 3 (cons 4 '())))
(cdr (cons 3 (cons 4 '())))]



@section{Pattern matching}

OCaml has a very nice pattern matching for letting you express case
analysis and decomposition in a concise way.

Each pattern maching expression has a sub-expression that produce a
value to be matched against and a number of clauses.  Each clause has
a pattern and an expression.  The pattern potentially consists of data
constructors, variables, and literals.  If the value matches the first
pattern, meaning the value and the template match up on constructors
and literals, then the variables are bound to the correspond parts of
the value, and the right-hand side expression is evaluated.  If the
value doesn't match, the next pattern is tried, and so on.  It's an
error if none of the patterns match.

So for example, we can write a functiion that recognize even digits as:

@ocaml-repl{
# let even_digit n =
    match n with
    | 0 -> true
    | 2 -> true
    | 4 -> true
    | 6 -> true
    | 8 -> true
    | _ -> false;;
val even_digit : int -> bool = <fun>
}

The patterns here, save the last one, are just integer literals.  If
@tt{n} is the same as any of these integers, the value @tt{true} is
produced.  The last case uses a "wildcard," which matches anything and
produces true.

Here's an example that matches a tuple, binding each part of the tuple
to a name and then using those names to construct a different tuple:

@ocaml-repl{
# let swap p =
    match p with
    | (x, y) -> (y, x);;
val swap : 'a * 'b -> 'b * 'a = <fun>
}

Here the pattern uses a data constructor (the tuple constructor).  It
matches any value that is made with the same constructor.

Here is a recursive function for computing the sum of a list of
numbers, defined with pattern matching:

@ocaml-repl{
# let rec sum xs =
    match xs with 
    | [] -> 0
    | x :: xs -> x + (sum xs);;
val sum : int list -> int = <fun>
# sum [4; 5; 6];;
- : int = 15
}


We can do the same in Racket:

@ex[
(define (even-digit n)
  (match n
    [0 #t]
    [2 #t]
    [4 #t]
    [6 #t]
    [8 #t]
    [_ #f]))

(define (swap p)
  (match p
    [(cons x y) (cons y x)]))

(define (sum xs)
  (match xs
    ['() 0]
    [(cons x xs)
     (+ x (sum xs))]))

(sum (list 4 5 6))
]


@section{Datatypes}

OCaml has the ability to declare new datatypes.  For example,
we can define type for binary trees of numbers:

@ocaml-repl{
# type bt = 
   | Leaf
   | Node of int * bt * bt;;
type bt = Leaf | Node of int * bt * bt
}

This declares a new type, named @tt{bt}. There are two
@emph{variants} of the @tt{bt} type, each with their own
constructor: @tt{Leaf} and @tt{Node}. The @tt{Leaf}
constructor takes no arguments, so just writing @tt{Leaf}
creates a (empty) binary tree:

@ocaml-repl{
# Leaf;;
- : bt = Leaf
}

The @tt{Node} constructor takes three arguments: an integer
and two binary trees. Applying the constructor to a tuple of
three things, makes a (non-empty) binary tree:

@ocaml-repl{
# Node (3, Leaf, Leaf);;
- : bt = Node (3, Leaf, Leaf)
}

Binary trees are an example of a @emph{recursive} datatype,
since one of the variants contains binary trees. This means
we can build up arbitrarily large binary trees by nesting
nodes within nodes:

@ocaml-repl{
# Node (3, Node (4, Leaf, Leaf), Node (7, Leaf, Leaf));;
- : bt = Node (3, Node (4, Leaf, Leaf), Node (7, Leaf, Leaf))
}

Pattern matching is used to do case analysis and deconstruct
values. So for example, a function that determines if a
binary tree is empty can be written as:

@ocaml-repl{
# let bt_is_empty bt = 
    match bt with
    | Leaf -> true
    | Node _ -> false;;
val bt_is_empty : bt -> bool = <fun>
# bt_is_empty Leaf;;
- : bool = true
# bt_is_empty (Node (3, Leaf, Leaf));;
- : bool = false
}

The patterns use the constructor names to discriminate on
which constructor was used for a given binary tree. The use
of the wildcard here is just saying it doesn't matter what's
inside a node; if you're a node, you're not empty.

Recursive functions work similarly, but use variables inside
patterns to bind names to the binary trees contained inside
a node:

@ocaml-repl{
# let rec bt_height bt =
    match bt with
    | Leaf -> 0
    | Node (_, left, right) -> 
      1 + max (bt_height left) (bt_height right);;
val bt_height : bt -> int = <fun>
# bt_height Leaf;;
- : int = 0
# bt_height (Node (4, Node (2, Leaf, Leaf), Leaf));;
- : int = 2
}

We do something very similar in Racket using @emph{
 structures}. A structure type is like a (single) variant of
a data type in OCaml: it's a way of combining several things
into one new kind of value.

@ex[
(struct leaf ())
(struct node (i left right))
]

This declares two new kinds of values: leaf structures and
node structures. For each, we get a constructor, which is a
function named after the structure type. The @racket[leaf]
constructor takes no arguments. The @racket[node]
constructor takes 3 arguments.

@ex[
(leaf)
(node 5 (leaf) (leaf))
(node 3 (node 2 (leaf) (leaf)) (leaf))
]

There is no type system in Racket, but we can conceptually still
define what we mean in a comment.  Just like in OCaml, we can use
pattern matching to discriminate and deconstruct:

@ex[
(code:comment "type Bt = (leaf) | (node Integer Bt Bt)")    
(define (bt-empty? bt)
  (match bt
    [(leaf) #t]
    [(node _ _ _) #f]))
(bt-empty? (leaf))
(bt-empty? (node 5 (leaf) (leaf)))
(define (bt-height bt)
  (match bt
    [(leaf) 0]
    [(node _ left right)
     (+ 1 (max (bt-height left)
               (bt-height right)))]))
(bt-height (leaf))
(bt-height (node 4 (node 2 (leaf) (leaf)) (leaf)))
]
 
@section{Symbols}

One of the built-in datatypes we will use often in Racket is
that of a @emph{symbol}. A symbol is just an atomic peice of
data. A symbol is written using the @racket[quote] notation
@racket[(code:quote symbol-name)], which is abbreviated
@racket['symbol-name]. What's allowable as a symbol name
follows the same rules as what's allowable as a Racket
identifier.


Symbols don't have a whole lot of operations. The main thing
you do with symbols is tell them apart from eachother:

@ex[
(equal? 'fred 'fred)
(equal? 'fred 'wilma)
]

It is possible to convert between symbols and strings:

@ex[
(symbol->string 'fred)
(string->symbol "fred")
]

There's also a convient function that produces a symbol that is guaranteed
to have not been used so far each time you call it:

@ex[
(gensym)
(gensym)
(gensym)
]


They can be used to define ``enum'' like datatypes:

@ex[
(code:comment "type Flintstone = 'fred | 'wilma | 'pebbles")
]


You can use pattern matching to match symbols:

@ex[
(define (flintstone? x)
  (match x
    ['fred #t]
    ['wilma #t]
    ['pebbles #t]
    [_ #f]))

(flintstone? 'fred)
(flintstone? 'barney)
]

There's really not a precise analog to symbols in OCaml.
(There's something called a polymorphic variant, which plays
a similar role to symbols in OCaml, but it wasn't covered in
CMSC 330.)



@;{
For example, suppose we want to build up binary trees using
polymorphic variants.  (This is not the idiomatic OCaml programming,
which would instead use a plain old variant and datatype, but it
corresponds more closely to the style of Racket code we will write.)

@ocaml-repl{
# `Leaf;;
- : [> `Leaf ] = `Leaf
# `Node (5, `Leaf, `Leaf);;
- : [> `Node of int * [> `Leaf ] * [> `Leaf ] ] = `Node (5, `Leaf, `Leaf)
}

We can define a datatype for binary trees:

@ocaml-repl{
# type bt = [ `Leaf | `Node of int * bt * bt ];;
type bt = [ `Leaf | `Node of int * bt * bt ]
}

And functions operating on binary trees can be defined by pattern matching:

@ocaml-repl{
# let rec sum bt = 
    match bt with
    | `Leaf -> 0
    | `Node (n, left, right) -> 
      n + sum left + sum right;;
val sum : ([< `Leaf | `Node of int * 'a * 'a ] as 'a) -> int = <fun>
}

To use the @tt{sum} function:
@ocaml-repl{
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
}

@section{Quote, quasiquote, and unquote}

One of the distinguishing features of languages in the Lisp family
(such as Scheme and Racket) is the @racket[quote] operator and its
closely related cousins @racket[quasiquote], @racket[unquote], and
@racket[unquote-splicing].

Let's start with @racket[quote].

The ``tick'' character @racket['d] is used as a shorthand for
@racket[(code:quote d)].

You've already seen it show up with symbols: @racket['x] is the symbol
@tt{x}.  It also shows up in the notation for the empty list:
@racket['()].

But you can also write @racket[quote] around non-empty lists like
@racket['(x y z)].  This makes a list of symbols.  It is equivalent to
saying @racket[(list 'x 'y 'z)].

In fact, you can nest lists within the quoted list: @racket['((x) y (q
r))].  This is equivalent to @racket[(list (list 'x) 'y (list 'q 'r))].

Here's another: @racket['(() (()) ((())))].  This is equivalent to
@centered{
@racket[(list '() (list '()) (list (list '())))]
}

So, anything you can write with quoted lists, you can write without
quoted lists by pushing the quote inward until reaching a symbol or an
empty set of parenthesis.

You can also put strings, booleans, and numbers inside of a
@racket[quote].  As you push the quote inward, it simply disappears
when reaching a string, boolean or number.  So @racket['5] is just
@racket[5].  Likewise @racket['#t] is @racket[#t] and @racket['"Fred"]
is @racket["Fred"].

You can also write pairs with @racket[quote], which uses the @tt{.}
notation for separating the left and right part of the pair.  For
example, @racket['(1 . 2)] is equivalent to @racket[(cons 1 2)].  If
you write something like @racket['(1 2 3 . 4)], what you are in effect
saying is @racket[(cons 1 (cons 2 (cons 3 4)))], an improper list that
ends in @racket[4].


In essence, @racket[quote] is a shorthand for conveniently
constructing data and is a very concise notation for writing down
ad-hoc data.  It serves much the same purpose as formats like JSON and
XML, except there's even less noise.

To summarize, with @racket[quote], you can construct

@itemlist[
@item{strings}
@item{booleans}
@item{numbers}
@item{symbols}
@item{and... pairs (or lists) of those things (including this one)}
]


The kind of things you can construct with the @racket[quote] form are
often called @bold{s-expressions}, short for @bold{symbolic
expressions}.

We can give a type definition for s-expressions:

@#reader scribble/comment-reader
(ex
;; type S-Expr =
;; | String
;; | Boolean
;; | Number
;; | Symbol
;; | (Listof S-Expr)
)

The reason for this name is because anything you can write
down as an expression, you can write down inside a
@racket[quote] to obtain @emph{a data representation} of
that expression. You can render an expression as a symbolic
representation of itself.

For example, @racket[(+ 1 2)] is an expression.  When run, it applies
the @emph{function} bound to the variable @racket[+] to the arguments
@racket[1] and @racket[2] and produces @racket[3].  On the other hand:
@racket['(+ 1 2)] constructs a peice of data, namely, a list of three
elements.  The first element is the @emph{symbol} @tt{+}, the second
element is @racket[2], the third element is @racket[3].


We will be using (subsets of) s-expressions extensively as our data
representation of AST and IR expressions, so it's important to gain a
level of fluency with them now.


Once you understand @racket[quote], moving on to @racket[quasiquote],
@racket[unquote], and @racket[unquote-splicing] are pretty
straight-forward.

Let's start with @racket[quasiquote]. The ``backtick''
character @racket[`d] is used as a shorthand for @tt{
 (quasiquote d)} and the ``comma'' character @racket[,e] is
shorthand for @tt{(unquote e)}. The @tt{(quasiquote d)} form
means the same thing as @tt{(quote d)}, with the exception
that if @tt{(unquote e)} appears anywhere inside @tt{d},
then the @emph{expression} @racket[e] is evaluated and it's
value will be used in place of @tt{(unquote e)}.

This gives us the ability to ``escape'' out of a quoted
peice of data and go back to expression mode.

If we think of @racket[quasiquote] like @racket[quote] in
terms of ``pushing in'' then the rules are exactly the same
except that when a @racket[quasiquote] is pushed up next to
an @racket[unquote], the two ``cancel out.'' So @racket[`,e] is
just @tt{e}.  

For example, @racket[`(+ 1 ,(+ 1 1))] is equivalent to
@racket[(list '+ 1 (+ 1 1))], which is equivalent to
@racket[(list '+ 1 2)].

So if @racket[quote] signals us to stop interpreting things
as expressions, but instead as data, @racket[quasiquote]
signals us to stop interpreting things as expression, but
instead as data.. @emph{unless we encounter a
 @racket[unquote]}, in which case you go back to interpreting
things as expressions.


The last remaining peice is @racket[unquote-splicing], which
is abbreviated with ``comma-at'': @racket[,@e] means @tt{
 (unquote-splicing e)}. The @racket[unquote-splicing] form is
like @racket[unquote] in that if it occurs within a
@racket[quasiquote], it means we switch back in to
expression mode. The difference is the expression must
produce a list (or pair) and the elements of that list (or
pair) are spliced in to the outer data.

So for example, @racket[`(+ 1 ,@(map add1 '(2 3)))] is
equivalent to
@racket[(cons '+ (cons 1 (map add1 (list 2 3))))], which is
equivalent to @racket[(list '+ 1 3 4)], or
@racket['(+ 1 3 4)].

If the expression inside the @racket[unquote-splicing]
produces something other than a pair, an error is signalled.

@section{Poetry of s-expressions}

The use of structures lets us program in a style very
similar to idiomatic OCaml programming. For each variant
data type, we can define a structure type for each variant
and use pattern matching to process such values.

However, we are going to frequently employ a different idiom
for programming with recursive variants which doesn't rely
on structures, but rather uses symbols in place of
constructors and lists in place of fields.

Let's revisit the binary tree example, using this style.

Notice that @racket[leaf] structureq is a kind of atomic
data. It doesn't contain anything and its only real purpose
is to be distinguishable from @racket[node] structures. On
the other hand a @racket[node] structure needs to be
distinguishable from @racket[leaf]s, but also contain 3
peices of data within it.

We can formulate definition of binary trees using only
symbols and lists as:

@ex[
(code:comment "type Bt = 'leaf | (list 'node Integer Bt Bt)")
]

So the following are binary trees:

@ex[
'leaf
(list 'node 3 'leaf 'leaf)
(list 'node 3
      (list 'node 7 'leaf 'leaf)
      (list 'node 9 'leaf 'leaf))
]

This formulation has the added benefit that we write binary trees
as s-expressions:

@ex[
'leaf
'(node 3 leaf leaf)
'(node 3
       (node 7 leaf leaf)
       (node 9 leaf leaf))
]

We re-write our functions to match this new datatype definition:

@ex[
(define (bt-empty? bt)
  (match bt
    ['leaf #t]
    [(cons 'node _) #f]))
(bt-empty? 'leaf)
(bt-empty? '(node 3
                  (node 7 leaf leaf)
                  (node 9 leaf leaf)))
(define (bt-height bt)
  (match bt
    ['leaf 0]
    [(list 'node _ left right)
     (+ 1 (max (bt-height left)
               (bt-height right)))]))
(bt-height 'leaf)
(bt-height '(node 3
                  (node 7 leaf leaf)
                  (node 9 leaf leaf)))]

We even can use @racket[quasiquote] notation in patterns to write
more concise definitions:

@ex[
(define (bt-empty? bt)
  (match bt
    [`leaf #t]
    [`(node . ,_) #f]))
(bt-empty? 'leaf)
(bt-empty? '(node 3
                  (node 7 leaf leaf)
                  (node 9 leaf leaf)))
(define (bt-height bt)
  (match bt
    [`leaf 0]
    [`(node ,_ ,left ,right)
     (+ 1 (max (bt-height left)
               (bt-height right)))]))
(bt-height 'leaf)
(bt-height '(node 3
                  (node 7 leaf leaf)
                  (node 9 leaf leaf)))]

Moreover, we can embrace quasiquotation at the type-level and write:

@ex[
(code:comment "type Bt = `leaf | `(node ,Integer ,Bt ,Bt)")
]

@section{Testing, modules, submodules}

We will take testing seriously in this class.  Primarily this will
take the form of unit tests, for which we will use the
@racketmodname[rackunit] library.  To use the library, you must
@racket[require] it.

Here is a simple example:

@ex[
(require rackunit)
(check-equal? (add1 4) 5)
(check-equal? (* 2 3) 7)
]

The @racket[check-equal?] function takes two arguments (and an
optional third for a message to display should the test fail) and
checks that the first argument produces something that is
@racket[equal?] to the expected outcome given as the second argument.

There are many other forms of checks and utilities for building up
larger test suites, but @racket[check-equal?] will get us a long way.

As a matter of coding style, we will place tests nearby the function
they are testing and locate them within their own @bold{module}.
Let's talk about modules for a minute.

In Racket, a module is the basic unit of code organization.  Every
file is a module whose name is derived from the filename, but you can
also write modules without saving them in a file.  For example:

@ex[
(module bt racket
  (provide bt-height)
  (define (bt-height bt)
    (match bt
      [`leaf 0]
      [`(node ,_ ,left ,right)
       (+ 1 (max (bt-height left)
                 (bt-height right)))])))
]

This declares a module named @racket[bt].  It provides a single value
named @racket[bt-height].

We can require the module from the REPL to gain access to the modules
provided values:

@ex[
(require 'bt)
(bt-height 'leaf)
]

We could have also used the @tt{#lang racket} shorthand for
@tt{(module bt racket ...)} and saved this in a file called
@tt{bt.rkt}.  To import from a file in the current directory, you'd
write @tt{(require "bt.rkt")}.  But this doesn't work well in REPL.

For the most part we will organize our programs into single module
files using the @tt{#lang racket} shorthand.  But we will place tests
within a ``sub''-module, i.e. a module nested inside of the module
that contains the code it tests.  We will use a special form called
@racket[module+] which declares a submodule that has access to the
enclosing module.  Moreover, repeated uses of @racket[module+] will
add content to the submodule.  By convention, we will name the testing
submodule @racket[test].

So here's a second version of the @racket[bt] module with unit tests
included (and more code).  Note the use of @racket[all-defined-out] to
provide everything:

@ex[
(module bt2 racket
  (code:comment "provides everything defined in module")
  (provide (all-defined-out))

  (module+ test
    (require rackunit))

  (define (bt-empty? bt)
    (match bt
      ['leaf #t]
      [(cons 'node _) #f]))

  (module+ test
    (check-equal? (bt-empty? 'leaf) #t)
    (check-equal? (bt-empty? '(node 3
                                    (node 7 leaf leaf)
                                    (node 9 leaf leaf)))
                  #f))

  (define (bt-height bt)
    (match bt
      [`leaf 0]
      [`(node ,_ ,left ,right)
       (+ 1 (max (bt-height left)
                 (bt-height right)))]))

  (module+ test
    (check-equal? (bt-height 'leaf) 0)
    (code:comment "intentionally wrong test:")
    (check-equal? (bt-height '(node 3 leaf leaf)) 2)))
]

Requiring this module with make @racket[bt-height], but @emph{it will not run the tests}:

@ex[
(require 'bt2)
]

Running the tests only happens when the @racket[test] submodule is required:

@ex[
(require (submod 'bt2 test))
]

Putting it all together, we can write the following code and save it
in a file called @tt{bt.rkt}.  (You can right-click the file name and
save the code to try it out.)

@codeblock-include["intro/bt.rkt"]

This code follows a coding style that we will use in this course:
@itemlist[
@item{it's organized in a module,}
@item{data type definitions occur at the top of the file,}
@item{it uses a test submodule to group unit tests,}
@item{tests occur immediately after the functions they test,}
@item{functions are annotated with type signatures and short purpose statements, and}
@item{indentation follows standard conventions (which DrRacket can apply for you).}
]

From the command line, you can run a module's tests using the Racket
command line testing tool @tt{raco test}:

@shellbox["raco test bt.rkt"]

Or simply give a directory name and test everything within that directory:

@shellbox["raco test ."]



