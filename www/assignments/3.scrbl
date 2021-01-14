#lang scribble/manual
@title[#:tag "Assignment 3" #:style 'unnumbered]{Assignment 3: Conditional forms and parsing}

@(require (for-label (except-in racket ...)))
@(require "../../langs/con-plus/semantics.rkt")
@(require redex/pict)

@bold{Due: Tues, Sept 29, 11:59PM}

@(define repo "https://classroom.github.com/a/YH2mwniz")

The goal of this assignment is to extend a compiler with some simple
unary numeric operations and conditional expressions, and to write a
parser.

Assignment repository:
@centered{@link[repo repo]}

You are given a repository with a starter compiler for the Con
language we studied in class.  You are tasked with:

@itemlist[
@item{extending the language in a number of ways and}
@item{implementing a parser for the language}
]

@section[#:tag-prefix "a3-" #:style 'unnumbered]{More primitives}

Add the following forms of expression to the Con language:

@itemlist[
@item{@racket[(abs _e)]: compute the absolute value of @racket[_e], and}
@item{@racket[(- _e)]: flips the sign of @racket[_e], i.e. compute @math{0-@racket[_e]}.}
]

Here's one possible way to compute the absolute value of the value in @tt{rax}:

@verbatim{
mov rbx, rax
neg rax
cmovl rax, rbx
}

To do this, you should:
@itemlist[
@item{Study @tt{ast.rkt} and the new forms of expression (i.e. new AST nodes)
      then update the comment at the top describing what the grammmar should look like.}
@item{Study @tt{syntax.rkt} and make sure you understand `expr?` and `sexpr->ast`.}
@item{Update @tt{interp.rkt} to correctly interpret these expressions.}
@item{Update @tt{compile.rkt} to correctly compile these expressions.}
]

The @tt{neg} and @tt{cmovl} instructions have been included in the
given @tt{asm} code.  If you need other x86 instructions, you will
need to modify the @tt{asm/*} code.


@section[#:tag-prefix "a3-" #:style 'unnumbered]{Con with Cond}

The Con language we studied added a simple form of performing
conditional evaluation of sub-expressions:

@racketblock[
(if (zero? _e0) _e1 _e2)
]

However, in the original paper on Lisp,
@link["http://jmc.stanford.edu/articles/recursive.html"]{@emph{Recursive
Functions of Symbolic Expressions and Their Computation by Machine,
Part I}}, John McCarthy introduced a generalization of @racket[if]
called ``conditional expressions,'' which we could add to our
language with the following syntax:

@racketblock[
(cond [(zero? _e-p1) _e-a1]
      ...
      [else _e-an])
]

A @racket[cond] expression has any number of clauses @racket[[(zero?
_e-pi) _e-ai] ...], followed by an ``else'' clause @racket[[else
_en]].  (We are using @racket[zero?] here to avoid having to
introduce booleans as a distinct data type; a restriction we
will remove later.)

The meaning of a @racket[cond] expression is computed by evaluating
each @racket[(zero? _e-pi)] in order until the first one that is true
is found, in which case, the corresponding @racket[_e-ai] is evaluated
and its value is the value of the @racket[cond] expression.  If no
such @racket[_e-pi] exists, @racket[_e-an]'s value is the value of the
@racket[cond].

The formal semantics can be defined as:

@(define ((rewrite s) lws)
   (define lhs (list-ref lws 2))
   (define rhs (list-ref lws 3))
   (list "" lhs (string-append " " (symbol->string s) " ") rhs ""))

@(require (only-in racket add-between))
@(define-syntax-rule (show-judgment name i j)
   (with-unquote-rewriter
      (lambda (lw)
        (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (lw-column lw) (lw-column-span lw)))
      (with-compound-rewriters (['+ (rewrite '+)]
                                ['- (rewrite '–)]
                                ['= (rewrite '=)]
				['!= (rewrite '≠)])
        (apply centered
	   (add-between 
             (build-list (- j i)
	                 (λ (n) (begin (judgment-form-cases (list (+ n i)))
	                               (render-judgment-form name))))
             (hspace 4))))))

@(show-judgment 𝑪 0 1)
@(show-judgment 𝑪 1 2)


Your task is to extend Con with this (restricted) form of @racket[cond].

To do this, you should:

@itemlist[
@item{Study @tt{ast.rkt} to understand the @racket[cond-e] and @racket[clause] AST nodes.}
@item{Study @tt{syntax.rkt} and make sure you understand the `cond-e` and `clause` aspects
      of `expr?` and `sexpr->ast`.}
@item{Update @tt{interp.rkt} to correctly interpret @racket[cond] expressions.}
@item{Update @tt{compile.rkt} to correctly compile @racket[cond] expressions.}
]

The subset of x86 needed to compile this extension of Con should not
require anything more than what was used for Con without
@racket[cond], so you should not need make changes to @tt{asm/*}.

@section[#:tag-prefix "a3-" #:style 'unnumbered]{Reading is Overrated}

We have so far side-stepped the issue of parsing by (1) relying on s-expression
notation for the concrete syntax of programs and (2) using the built-in
@racket[read] to get an s-expression and (3) then using the @racket[sexpr->ast]
function that we've written to get our AST.

Your task is to design and implement a parser for the extended Con
language based on the following grammar:

@verbatim{
<expr> ::= integer
        |  ( <compound> )
        |  [ <compound> ]

<compound> ::= <prim> <expr>
            |  if <question> <expr> <expr>
            |  cond <clause>* <else>
	
<prim> ::= add1 | sub1 | abs | -

<clause> ::= ( <question> <expr> )
          |  [ <question> <expr> ]

<question> ::= ( zero? <expr> )
            |  [ zero? <expr> ]

<else> ::= ( else <expr> )
        |  [ else <expr> ]
}

There is a lexer given to you in @tt{lex.rkt}, which provides two
functions: @racket[lex-string] and @racket[lex-port], which consume a
string or an input port, respectively, and produce a list of tokens,
which are defined as:

@#reader scribble/comment-reader
(racketblock
;; type Token =
;; | Integer
;; | 'add1
;; | 'sub1
;; | 'zero?
;; | 'if
;; | 'cond
;; | 'else
;; | 'abs
;; | '-
;; | 'lparen    ;; (
;; | 'rparen    ;; )
;; | 'lsquare   ;; [
;; | 'rsquare   ;; ]
;; | 'eof       ;; end of file
)

The lexer will take care of reading the @tt{#lang racket} header and
remove any whitespace.

You must complete the code in @tt{parse.rkt} to implement the parser
which constructs an s-expression representing a valid (extended) Con
expression, if possible, from a list of tokens.  The @racket[parse]
function should have the following signature and must be provided by
the module:

@#reader scribble/comment-reader
(racketblock
;; parse : [Listof Token] -> Expr
)

As an example, @racket[parse] should produce @racket[(add1-e (sub1-e (int-e 7)))]
if given @racket['(lparen add1 lparen sub1 7 rparen rparen eof)].

You should not need to make any changes to @tt{lex.rkt}.

You may use any approach you'd like to write the parser, but following
the recursive descent predictive parsing as studied in CMSC 330 is
recommended.  See the
@link["http://www.cs.umd.edu/class/spring2019/cmsc330/lectures/04-parsing.pdf"]{slides}
if you need a refresher.

If you want to set things up as done in 330, you can do the following:

@#reader scribble/comment-reader
(racketblock
(define *input* (box '()))

;; [Listof Token] -> Expr
(define (parse lot)
  (set-box! *input* lot)
  (let ((e (parse-expr!))
        (_ (match-tok! 'eof)))
    e))

;; -> Expr
;; EFFECT: consume one expression's worth of tokens
(define (parse-expr!)
  (match (look-ahead)
    [... ...]))

;; -> Token
;; Produce (but don't consume) the next token
(define (look-ahead)
  (match (unbox *input*)
    ['() (error "no look ahead available")]
    [(cons t _) t]))

;; Token -> Token
;; EFFECT: consumes one token of input
(define (match-tok! t)
  (match (unbox *input*)
    ['() (error "no token available")]
    [(cons next ts)
     (set-box! *input* ts)
     (unless (equal? t next)
       (error "parse error"))
     t]))
)

The @racket[box], @racket[unbox], and @racket[set-box!] functions
correspond to OCaml's @tt{ref}, @tt{!}, and @tt{:=} operators,
respectively.

The @tt{bang!} naming convention is a Scheme convention for marking
effectful functions (but it's just a naming convention).

This construction closely follows the 330 notes.

There is one complication, which is that the grammar requires 2 tokens
of look-ahead when parsing a @racket[cond] in order to determine if
the next thing to parse is a @tt{<clause>} or an @tt{<else>}.

The simplest solution is just to add a @racket[look-ahead2] function
that let's you peek at the second token in the input stream.

As an alternative to the 330 design, you could try to do things
functionally with the following set-up:

@#reader scribble/comment-reader
(racketblock
;; [Listof Token] -> Expr
(define (parse lot)
  (match (parse-expr lot)
    [(cons '(eof) e) e]
    [_ (error "parse error")]))

;; [Listof Token] -> (Pairof [Listof Token] Expr)
(define (parse-expr lot)
  (match lot
    [... ...]))
)

Here the idea is that each function that corresponds to a non-terminal
is given the list of tokens to parse.  It produces a pair of things:
the remaining tokens after parsing and the thing it parsed.  (The
functional approach is much easier to test, IMO.)


Once your parser is complete, you can make the noted changes in
@tt{compile-file.rkt} and @tt{interp-file.rkt} to make use of your own
parser and remove the dependence on Racket's @racket[read] function.

@section[#:tag-prefix "a3-" #:style 'unnumbered]{Testing}

You can test your code in several ways:

@itemlist[

 @item{Using the command line @tt{raco test .} from
  the directory containing the repository to test everything.}

 @item{Using the command line @tt{raco test <file>} to
  test only @tt{<file>}.}

 @item{Pushing to github. You can 
  see test reports at:
  @centered{@link["https://travis-ci.com/cmsc430/"]{
    https://travis-ci.com/cmsc430/}}

  (You will need to be signed in in order see results for your private repo.)}]

Note that only a small number of tests are given to you, so you should
write additional test cases.

Ther @tt{random.rkt} module provides a @racket[random-expr] function
for generating random (extended) Con expressions.  It is used in the
@tt{test/compile-rand.rkt} file to randomly test compiler correctness.

There is a property-based random tester for the compiler in
@tt{test/compile-rand.rkt} that compiles and runs 500 random programs.

@section[#:tag-prefix "a3-" #:style 'unnumbered]{Submitting}

Pushing your local repository to github ``submits'' your work.  We
will grade the latest submission that occurs before the deadline.

