#lang scribble/manual
@title[#:tag "Assignment 3" #:style 'unnumbered]{Assignment 3: Primitives, Conditionals, and Dispatch}

@(require (for-label (except-in racket ...)))
@(require "../../langs/con-plus/semantics.rkt")
@(require redex/pict)

@bold{Due: Thu, Sept 29, 11:59PM}

The goal of this assignment is to extend the parser, interpreter, and
compiler with some simple unary numeric and boolean operations and two
forms of control flow expressions: @racket[cond]-expressions and
@racket[case]-expressions.


You are given a zip file on ELMS with a starter compiler based on the
Dupe language we studied in class.  You are tasked with extending the
language in a number of ways:

@itemlist[
@item{adding new primitive operations,}
@item{adding @racket[cond], and}
@item{adding @racket[case].}
]

You may use any a86 instructions you'd like, however it is possible to
complete the assignment using @racket[Cmp], @racket[Je], @racket[Jg],
@racket[Jmp], @racket[Label], @racket[Mov], and @racket[Sub].

@section[#:tag-prefix "a3-" #:style 'unnumbered]{More primitives}

Add the following forms of expression to the language:

@itemlist[
@item{@racket[(abs _e)]: compute the absolute value of @racket[_e],}
@item{@racket[(- _e)]: flips the sign of @racket[_e], i.e. compute @math{0-@racket[_e]}, and}
@item{@racket[(not _e)]: compute the logical negation of @racket[_e]; note that the negation of @emph{any} value other than @racket[#f] is @racket[#f] and the negation of @racket[#f] is @racket[#t].}
]

There are many ways to implement these at the assembly level. You should try implementing
these using the limited a86 instruction set.

To do this, you should:
@itemlist[
@item{Study @tt{ast.rkt} and the new forms of expression (i.e. new AST nodes)
      then update the comment at the top describing what the grammmar should look like.}
      
@item{Study @tt{parse.rkt} and add support for parsing these
expressions. (See @secref[#:tag-prefixes '("a3-")]{parse} for guidance.)}

@item{Update @tt{interp-prim.rkt} and @tt{interp.rkt} to correctly interpret these expressions.}

@item{Make examples of these primitives and potential translations of them
to assembly.}

@item{Update @tt{compile.rkt} to correctly compile these expressions.}

@item{Check your implementation by running the tests in @tt{test/all.rkt}.}
]

@section[#:tag-prefix "a3-" #:style 'unnumbered]{Conditional Evaluation with Cond}

The Dupe language we studied included a simple form of performing
conditional evaluation of sub-expressions:

@racketblock[
(if _e0 _e1 _e2)
]

However, in the original paper on Lisp,
@link["http://jmc.stanford.edu/articles/recursive.html"]{@emph{Recursive
Functions of Symbolic Expressions and Their Computation by Machine,
Part I}}, John McCarthy introduced a generalization of @racket[if]
called ``conditional expressions,'' which we could add to our
language with the following syntax:

@racketblock[
(cond [_e-p1 _e-a1]
      ...
      [else _e-an])
]

A @racket[cond] expression has any number of clauses @racket[[_e-pi
_e-ai] ...], followed by an ``else'' clause @racket[[else _en]].  For
the purposes of this assignment, we will assume every @racket[cond]
expression ends in an @racket[else] clause, even though this is not
true in general for Racket.  The parser should reject any
@racket[cond]-expression that does not end in @racket[else].


The meaning of a @racket[cond] expression is computed by evaluating
each expression @racket[_e-pi] in order until the first one that
does not evaluate to @racket[#f] is found, in which case, the corresponding expression
@racket[_e-ai] is evaluated and its value is the value of the
@racket[cond] expression.  If no such @racket[_e-pi] exists, the
expression @racket[_e-an]'s value is the value of the @racket[cond].

@;{
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
}

Your task is to extend Dupe with this (restricted) form of @racket[cond].

To do this, you should:

@itemlist[
@item{Study @tt{ast.rkt} to add appropriate AST nodes.}
@item{Extend @tt{parse.rkt} to parse such expressions. (See @secref[#:tag-prefixes '("a3-")]{parse} for guidance.)}
@item{Update @tt{interp-prim.rkt} and @tt{interp.rkt} to correctly interpret @racket[cond] expressions.}

@item{Make examples of @racket[cond]-expressions and potential translations of them
to assembly.}

@item{Update @tt{compile.rkt} to correctly compile @racket[cond]
expressions based on your examples.}

@item{Check your implementation by running the tests in @tt{test/all.rkt}.}
]

@section[#:tag-prefix "a3-" #:style 'unnumbered]{Dispatching Evaluation with Case}


Racket has a mechanism for dispatching between a number of possible
expressions based on a value, much like C's notion of a
@tt{switch}-statement.  This is the @racket[case]-expression, which we
could add to our language with the following syntax:

@racketblock[
(case _ev
      [(_d1 ...) _e1]
      ...
      [else _en])
]

The meaning of a @racket[case] expression is computed by evaluating
the expression @racket[_ev] and then proceeding in order through each
clause until one is found that has a datum @racket[_di] equal to
@racket[_ev]'s value.  Once such a clause is found, the corresponding
expression @racket[_ei] is evaluated and its value is the value of the
@racket[case] expression.  If no such clause exists, expression
@racket[_en] is evaluated and its value is the value of the
@racket[case] expression.

Note that each clause consists of a parenthesized list of
@emph{datums}, which in the setting of Dupe means either integer or
boolean literals.

Your task is to extend Dupe with this (restricted) form of @racket[case].

To do this, you should:

@itemlist[
@item{Study @tt{ast.rkt} to add appropriate AST nodes.}
@item{Extend @tt{parse.rkt} to parse such expressions. (See @secref[#:tag-prefixes '("a3-")]{parse} for guidance.)}
@item{Update @tt{interp-prim.rkt} and @tt{interp.rkt} to correctly interpret @racket[case] expressions.}

@item{Make examples of @racket[case]-expressions and potential translations of them
to assembly.}

@item{Update @tt{compile.rkt} to correctly compile @racket[case] expressions based on your examples.}

@item{Check your implementation by running the tests in @tt{test/all.rkt}.}
]

@section[#:tag-prefix "a3-" #:style 'unnumbered #:tag "parse"]{A Leg Up on Parsing}

In the past, designing the AST type and structure definitions has
given students some grief.  Getting stuck at this point means you
can't make any progress on the assignment and making a mistake at this
level can cause real trouble down the line for your compiler.

For that reason, let us give you a strong hint for a potential design
of the ASTs and examples of how parsing could work.  You are not
required to follow this design, but you certainly may.

Here's a potential AST definition for the added primitives,
@racket[cond], and @racket[case]:

@#reader scribble/comment-reader
(racketblock
;; type Expr =
;; ...
;; | (Cond [Listof CondClause] Expr)
;; | (Case Expr [Listof CaseClause] Expr)

;; type CondClause = (Clause Expr Expr)
;; type CaseClause = (Clause [Listof Datum] Expr)

;; type Datum = Integer | Boolean

;; type Op = 
;; ...
;; | 'abs | '- | 'not

(struct Cond (cs e)    #:prefab)
(struct Case (e cs el) #:prefab)
(struct Clause (p b)   #:prefab)
)

There are two new kinds of expression constructors: @racket[Cond] and
@racket[Case].  A @racket[Cond] AST node contains a list of
cond-clauses and expression, which the expression of the @racket[else]
clause.  Each cond-clause is represented by a @racket[Clause]
structure containing two expressions: the right-hand-side of the
clause which is used to determine whether the left-hand-side is
evaluated, and the left-hand-side expression.

The @racket[Case] AST node contains three things: an expression that
is the subject of the dispatch (i.e. the expression that is evaluated
to determine which clause should be taken), a list of case-clauses
(not to be confused with cond-clauses), and an @racket[else]-clause
expression.  Each case-clause, like a cond-clause, consists of two
things.  Hence we re-use the @racket[Clause] structure, but with
different types of elements.  The first element is a list of
@emph{datums}, each being either an integer or a boolean.

Now, we won't go so far as to @emph{give} you the code for
@racket[parse], but we can give you some examples:

@itemlist[

@item{@racket[(abs 1)] parses as @racket[(Prim1 'abs (Int 1))],}

@item{@racket[(not #t)] parses as @racket[(Prim1 'not (Bool #t))],}

@item{@racket[(cond [else 5])] parses as @racket[(Cond '() (Int 5))],}

@item{@racket[(cond [(not #t) 3] [else 5])] parses as @racket[(Cond
(list (Clause (Prim1 'not (Bool #t)) (Int 3))) (Int 5))],}

@item{@racket[(cond [(not #t) 3] [7 4] [else 5])] parses as
@racket[(Cond (list (Clause (Prim1 'not (Bool #t)) (Int 3)) (Clause
(Int 7) (Int 4))) (Int 5))],}

@item{@racket[(case (add1 3) [else 2])] parses as @racket[(Case (Prim1
'add1 (Int 3)) '() (Int 2))].}

@item{@racket[(case 4 [(4) 1] [else 2])] parses as @racket[(Case (Int
4) (list (Clause (list 4) (Int 1))) (Int 2))],}

@item{@racket[(case 4 [(4 5 6) 1] [else 2])] parses as @racket[(Case (Int
4) (list (Clause (list 4 5 6) (Int 1))) (Int 2))], and}

@item{@racket[(case 4 [(4 5 6) 1] [(#t #f) 7] [else 2])] parses as @racket[(Case (Int
4) (list (Clause (list 4 5 6) (Int 1)) (Clause (list #t #f) (Int 7))) (Int 2))].}
]


@section[#:tag-prefix "a3-" #:style 'unnumbered]{Testing}

You can test your code in several ways:

@itemlist[

 @item{Using the command line @tt{raco test .} from
  the directory containing the repository to test everything.}

 @item{Using the command line @tt{raco test <file>} to
  test only @tt{<file>}.}
]

Note that only a small number of tests are given to you, so you should
write additional test cases.

@section[#:tag-prefix "a3-" #:style 'unnumbered]{Rubric}

The autograder will grade based on these eight components:

Handwritten Tests (proportionally graded)

@itemlist[

 @item{(50%) Public and non-public handwritten tests: Gradescope's @tt{all.rkt} will contain additional private tests that does not match the ones in the publicly given file}

]

Randomized Property Testing (must pass all the associated tests for each property)

@itemlist[

  @item{(6.25%) Correct intepreter for primitives}

 @item{(6.25%) Correct compiler for primitives}
 
 @item{(6.25%) Correct intepreter for cond}

 @item{(6.25%) Correct compiler for cond}
 
   @item{(6.25%) Correct intepreter for case}

 @item{(6.25%) Correct compiler for case}
 
   @item{(6.25%) Correct intepreter}

 @item{(6.25%) Correct compiler}
]


@section[#:tag-prefix "a3-" #:style 'unnumbered]{Submitting}

You should submit on Gradescope. You should submit a zip file with
exactly the same structure that the stub contains (a dupe-plus
folder). We will only use the @tt{parse.rkt}, @tt{ast.rkt},
@tt{compile.rkt}, @tt{interp.rkt}, and @tt{interp-prim.rkt} files for
grading, so make sure all your work is contained there! The autograder will fail if it does not contain a dupe-plus folder with these files or there a syntax error.
