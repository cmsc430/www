#lang scribble/manual
@title[#:tag "Assignment 3" #:style 'unnumbered]{Assignment 3: Primitives, Conditionals, and Dispatch}

@(require (for-label (except-in racket ...)))
@(require "../../langs/con-plus/semantics.rkt")
@(require redex/pict)

@bold{Due: Thu, Sept 30, 11:59PM}

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
@item{@racket[(not _e)]: compute the logical negation of @racket[_e]; note that the negation of @emph{any} value other than @racket[#f] is @racket[#t] and the negation of @racket[#f] is @racket[#t].}
]

There are many ways to implement these at the assembly level. You should try implementing
these using the limited a86 instruction set.

To do this, you should:
@itemlist[
@item{Study @tt{ast.rkt} and the new forms of expression (i.e. new AST nodes)
      then update the comment at the top describing what the grammmar should look like.}
@item{Study @tt{parse.rkt} and add support for parsing these expressions.}
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
each expression @racket[_e-pi] in order until the first one that is
true is found, in which case, the corresponding expression
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
@item{Extend @tt{parse.rkt} to parse such expressions.}
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
@item{Extend @tt{parse.rkt} to parse such expressions.}
@item{Update @tt{interp-prim.rkt} and @tt{interp.rkt} to correctly interpret @racket[case] expressions.}

@item{Make examples of @racket[case]-expressions and potential translations of them
to assembly.}

@item{Update @tt{compile.rkt} to correctly compile @racket[case] expressions based on your examples.}

@item{Check your implementation by running the tests in @tt{test/all.rkt}.}
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

@section[#:tag-prefix "a3-" #:style 'unnumbered]{Submitting}

You should submit on Gradescope. You should submit a zip file with
exactly the same structure that the stub contains (a dupe-plus
folder). We will only use the @tt{parse.rkt}, @tt{ast.rkt},
@tt{compile.rkt}, @tt{interp.rkt}, and @tt{interp-prim.rkt} files for
grading, so make sure all your work is contained there!
