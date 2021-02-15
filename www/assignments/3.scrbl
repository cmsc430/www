#lang scribble/manual
@title[#:tag "Assignment 3" #:style 'unnumbered]{Assignment 3: Conditional forms and parsing}

@(require (for-label (except-in racket ...)))
@(require "../../langs/con-plus/semantics.rkt")
@(require redex/pict)

@(define repo "https://github.com/cmsc430/assign03/tree/spring2021")

@bold{Due: Thu, Feb 25, 11:59PM}

The goal of this assignment is to extend the parser, interpreter, and
compiler some simple unary numeric numeric and boolean operations and
conditional expressions.

Assignment repository:
@centered{@link[repo repo]}

You are given a repository with two starter compilers, one for the Con
and one for the Dupe language we studied in class.  You are tasked
with extending each language in a number of ways:

@section[#:tag-prefix "a3-" #:style 'unnumbered]{Con - More primitives}

Add the following forms of expression to the Con language:

@itemlist[
@item{@racket[(abs _e)]: compute the absolute value of @racket[_e], and}
@item{@racket[(- _e)]: flips the sign of @racket[_e], i.e. compute @math{0-@racket[_e]}.}
]

There are many ways to implement these at the assembly level. You should try implementing
these using the limited a86 instruction set.

Hint: Use subtraction!

To do this, you should:
@itemlist[
@item{Study @tt{ast.rkt} and the new forms of expression (i.e. new AST nodes)
      then update the comment at the top describing what the grammmar should look like.}
@item{Study @tt{parse.rkt} and add support for parsing these expressions.}
@item{Update @tt{interp-prim.rkt} and @tt{interp.rkt} to correctly interpret these expressions.}
@item{Update @tt{compile.rkt} to correctly compile these expressions.}
]

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
@item{Study @tt{ast.rkt} to add @racket[Cond] and @racket[Clause] AST nodes.}
@item{Extend @tt{parse.rkt} to parse such expressions.}
@item{Update @tt{interp-prim.rkt} and @tt{interp.rkt} to correctly interpret @racket[cond] expressions.}
@item{Update @tt{compile.rkt} to correctly compile @racket[cond] expressions.}
]

The subset of x86 needed to compile this extension of Con should not
require anything more than what was used for Con without
@racket[cond].

@section[#:tag-prefix "a3-" #:style 'unnumbered]{Dupe+}

The last part of this assignment is to port these extensions over to
Dupe and add one more unary primitive, this time one that operates on
booleans.

@racket[(not _e)]: compute the boolean negation of @racket[_e]

a86 Hint: While you can implement this with a jump, it is possible
to implement using only a single a86 instruction :)

In addition to @tt{not}, you should also add the primitives you
implemented for Con (absolute value and negation), as well as
conditionals.

Conditionals can now be properly unrestricted: the checks no
longer need to be @tt{zero?} predicates, but can be arbitrary
expressions that evaluate to a boolean.

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
exactly the same structure that the stub contains (a con-plus and a
dupe-plus folder). We will only use the @tt{parse.rkt}, @tt{ast.rkt},
@tt{compile.rkt}, @tt{interp.rkt}, and @tt{interp-prim.rkt} files
for grading, so make sure all your work is contained there!
