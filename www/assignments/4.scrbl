#lang scribble/manual
@title[#:tag "Assignment 4" #:style 'unnumbered]{Assignment 4: Let there be (Many) Variables}

@(require (for-label (except-in racket ...)))
@(require "../../langs/fraud-plus/semantics.rkt")
@(require redex/pict)

@(require "../notes/ev.rkt")

@bold{Due: Tuesday, March 23rd at 11:59PM EST}

@(define repo "https://github.com/cmsc430/assign04")

The goal of this assignment is to extend a compiler with binding forms. 

Assignment repository:
@centered{@link[repo repo]}

You are given a repository with a starter compiler similar to the
@seclink["Fraud"]{Fraud} language we studied in class.  You are tasked
with:

@itemlist[

@item{incorporating the Con+ features you added in
@seclink["Assignment 3"]{Assignment 3},}

@item{extending the addition primitive to handle an arbitrary number of arguments,}

@item{extending the @racket[let]-binding form of the language to bind any number of variables, and}

@item{extending the @racket[let]-binding form of the language to allow back-references (@racket[let*]).}
]

@section[#:tag-prefix "a4-" #:style 'unnumbered]{From Con+ and Dupe+ to Fraud+}

Implement the @racket[abs], unary @racket[-] and @racket[not]
operations and the @racket[cond] form from @seclink["Assignment
3"]{Assignment 3}.

Unlike Assignment 3, the ast struct definitions and parsing code are
provided. Study the relevant parts in @tt{ast.rkt} and @tt{parse.rkt},
understand what is different (if anything) from your own
implementation and implement the relevant functionality in
@tt{interp.rkt}, @tt{interp-prim.rkt}, and @tt{compile.rkt}.  You can
start from your previous code, but you will need to update it to work
for the structures provided. What's essentially left for you to do is
to make sure to correctly signal an error (@racket['err]) when these
constructs are applied to the wrong type of argument.

While you're at it, implement the predicates @racket[integer?] and
@racket[boolean?] for checking the type of an argument, modeled by
@racket[char?] which was covered in the lectures.

In case it's helpful, the formal semantics of @racket[cond] are defined as:

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

@(show-judgment 𝑭-𝒆𝒏𝒗 0 1)
@(show-judgment 𝑭-𝒆𝒏𝒗 1 2)
@(show-judgment 𝑭-𝒆𝒏𝒗 2 3)


The following files have already been updated for you:
@itemlist[
@item{@tt{ast.rkt}}
@item{@tt{parse.rkt}} 
]

You will need to modify:
@itemlist[
@item{@tt{compile.rkt}}
@item{@tt{interp.rkt}}
@item{@tt{interp-prim.rkt}}
@item{@tt{types.rkt}}
]
to correctly implement these features.

You do not necessarily need to change all of these files depending on
your design choices, but you shouldn't alter any other files for
Gradescope to work. If you feel that modifying a different file leads
to a more natural/intuitive design - reach out! We'll be happy to see
a different approach and possibly extend our infrastructure to handle
that.

@section[#:tag-prefix "a4-" #:style 'unnumbered]{From Binary to Variadic Addition}

In Fraud, we implemented a binary operation for addition. However,
Racket supports an arbitrary number of arguments for @racket[+]. Your
job is to extend the parser, interpreter, and compiler to behave
similarly.

The following file have already been updated for you:

@itemlist[
@item{@tt{ast.rkt}}
]

You will need to modify
@itemlist[
@item{@tt{compile.rkt}}
@item{@tt{parse.rkt}}
@item{@tt{interp.rkt}}
@item{@tt{interp-prim.rkt}}
]
to correctly implement these features. 

@section[#:tag-prefix "a4-" #:style 'unnumbered]{Generalizing Let}

The Fraud language has a let form that binds a single variable in the
scope of some expression.  This is a restriction of the more general
form of @racket[let] that binds any number of expressions.  So for
example,

@racketblock[
(let ((x 1) (y 2) (z 3))
  _e)
]

simultaneously binds @racket[x], @racket[y], and @racket[z] in the
scope of @racket[_e].

The syntax of a @racket[let] expression allows any number of binders
to occur, so @racket[(let () _e)] is valid syntax and is equivalent to
@racket[_e].

The binding of each variable is only in scope in the body, @bold{not}
in the right-hand-sides of any of the @racket[let].

For example, @racketblock[(let ((x 1) (y x)) 0)] is a syntax error
because the occurrence of @racket[x] is not bound.

To capture that behavior, you need to implement:

@itemize[
@item{@code[#:lang "racket"]{well-formed? ; Expr -> Boolean} in @tt{parse.rkt},
which consumes
an expression and determines if it is a well-formed expression, i.e. it
must be an instance of an @tt{Expr} @emph{and} each @racket[let]
expression must bind a distinct set of variables, while the bodies of the
let-bindings only reference variables that are available in the context
prior to the let.
We have provided a new struct in @tt{ast.rkt}, @racket[IllFormedError]
that is raised by the parser when the expression read fails the
@racket[well-formed] check.
}

@item{The parser, interpeter, and compiler functionality to correctly
handle the generalized form of @racket[let].  The compiler may assume
the input is a @racket[well-formed] expression.}
]

@section[#:tag-prefix "a4-" #:style 'unnumbered]{Back-Referencing Let}

Similar to @racket[let] there is also @racket[let*] that also binds any number
of expressions. The difference is that previous bindings are available to
subsequent bindings. For example,

@racketblock[
(let* ((x 1) (y 2) (z (add1 y)))
  _e)
]

binds @racket[x] to 1, @racket[y] to 2, and @racket[z] to 3 in
the scope of @racket[_e].

The syntax of a @racket[let*] expression allows any number of binders
to occur, so @racket[(let* () _e)] is valid syntax and is equivalent to
@racket[_e].

Unlike @racket[let], @racketblock[(let* ((x 1) (y x)) 0)] is @emph{not} a syntax
error and should not produce an @racket[IllFormedError].

Update the parser, interpreter, and compiler to implement
this different form of let-binding.

@section[#:tag-prefix "a4-" #:style 'unnumbered]{Testing}

You can test your code in several ways:

@itemlist[

 @item{Using the command line @tt{raco test .} from
  the directory containing the repository to test everything.}

 @item{Using the command line @tt{raco test <file>} to
  test only @tt{<file>}.}
]

Note that only a small number of tests are given to you, so you should
write additional test cases.

@bold{We provide a @tt{random-exprs.rkt} module which provides
@racket[exprs], a list of 500 randomly-generated closed expressions. You can use
them freely to test various properties of your interpreter or compiler.}

@section[#:tag-prefix "a4-" #:style 'unnumbered]{Submitting}

You should submit on Gradescope. You should submit a zip file that has
exactly the same structure that the stub contains. We will only use
the @tt{parse.rkt}, @tt{compile.rkt}, @tt{interp.rkt}, @tt{types.rkt}
and @tt{interp-prim.rkt} files for grading, so make sure all your work
is contained there! Note the lack of @tt{ast.rkt} - part of assignment
3 was learning to design your own structures, part of assignment 4 is
learning to work within the constraints of an existing design!


