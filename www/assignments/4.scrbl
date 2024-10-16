#lang scribble/manual

@title[#:tag "Assignment 4" #:style 'unnumbered]{Assignment 4: Let There Be (Many) Variables}

@bold{Due: Tuesday, October 29, 11:59PM EST}

The goal of this assignment is to extend a compiler with binding forms and
primitives that can take any number of arguments.


@section[#:tag-prefix "a4-" #:style 'unnumbered]{Overview}

For this assignment, you are given a @tt{fraud-plus.zip} file on ELMS
with a starter compiler similar to the @seclink["Fraud"]{Fraud}
language we studied in class.

Unlike @seclink["Assignment 3"]{Assignment 3}, the following files have already
been updated for you @bold{and should not be changed by you}:
@itemlist[
@item{@tt{ast.rkt}}
@item{@tt{parse.rkt}}
]

So you will only need to modify:
@itemlist[
@item{@tt{interp.rkt}}
@item{@tt{interp-prim.rkt}}
@item{@tt{compile.rkt}}
@item{@tt{compile-ops.rkt}}
]
to correctly implement the new features. These features are described below.


@subsection[#:tag-prefix "a4-" #:style 'unnumbered]{Submitting}

Submit a zip file containing your work to Gradescope. Use @tt{make submit.zip}
from within the @tt{fraud-plus} directory to create a zip file with the proper
structure.

We will not use your @tt{ast.rkt} or @tt{parse.rkt} files. Part of Assignment 3
was learning to design your own structures, but part of Assignment 4 is
learning to work within the constraints of an existing design!


@subsection[#:tag-prefix "a4-" #:style 'unnumbered]{Testing}

You can test your code in several ways:

@itemlist[

 @item{Using the command line @tt{raco test test/} from the @tt{fraud-plus}
  directory to test everything.}

 @item{Using the command line @tt{raco test <file>} to only test @tt{<file>}.}
  ]

Note that only a small number of tests are given to you, so you should
write additional test cases. 


@section[#:tag-prefix "a4-" #:style 'unnumbered]{Fraud+}

The Fraud+ language extends the Fraud language we studied in class with some
new features:

@itemlist[

@item{The features added in @seclink["Assignment 3"]{Assignment 3}, namely:

  @itemlist[

  @item{@racket[abs], @racket[-], and @racket[not]}
  @item{@racket[cond]}
  @item{@racket[case]}

  ]}

@item{New primitives @racket[integer?] and @racket[boolean?].}

@item{An extended @racket[+] that accepts any number of arguments.}

@item{An extended @racket[let] that can bind multiple variables at once.}

@item{Back-referencing @racket[let*] that can bind multiple variables at once.}

]


@subsection[#:tag-prefix "a4-" #:style 'unnumbered]{From Dupe+ to Fraud+}

Implement the @racket[abs], unary @racket[-], and @racket[not] operations and
the @racket[cond] and @racket[case] forms from
@seclink["Assignment 3"]{Assignment 3} by modifying @tt{interp.rkt},
@tt{interp-prim.rkt}, @tt{compile.rkt}, and @tt{compile-ops.rkt}. You can
start from your previous code, but you will need to update it to work for the
structures provided. What's essentially left for you to do is to make sure to
correctly signal an error (@racket['err]) when these constructs are
applied to the wrong type of argument.

While you're at it, implement the predicates @racket[integer?] and
@racket[boolean?] for checking the type of an argument, modeled by the
@racket[char?] predicate that was covered in the lectures.


@subsection[#:tag-prefix "a4-" #:style 'unnumbered]{From Binary to Variadic Addition}

In Fraud, we implemented a binary operation for addition. However, Racket
supports an arbitrary number of arguments for @racket[+]. Your job is to extend
the interpreter and compiler to behave similarly.


@subsection[#:tag-prefix "a4-" #:style 'unnumbered]{Generalizing Let}

The Fraud language has a @tt{let} form that binds a single variable in the
scope of some expression. This is a restriction of the more general form of
@racket[let] that binds any number of expressions. So, for example,

@racketblock[
(let ((x 1) (y 2) (z 3))
  _e)
]

simultaneously binds @racket[x], @racket[y], and @racket[z] in the scope of
@racket[_e].

The syntax of a @racket[let] expression allows any number of binders to occur,
so @racket[(let () _e)] is valid syntax and is equivalent to @racket[_e].

The binding of each variable is only in-scope within the body, @bold{not} in
the right-hand sides of any of the @racket[let]. So, for example,
@racketblock[(let ((x 1) (y x)) 0)] is a syntax error because the occurrence of
@racket[x] is not bound.


@subsection[#:tag-prefix "a4-" #:style 'unnumbered]{Back-Referencing Let}

Similar to @racket[let], there is also @racket[let*] that can also bind any
number of expressions. The difference is that previous bindings are available
in the right-hand sides of subsequent bindings. For example,

@racketblock[
(let* ((x 1) (y 2) (z (add1 y)))
  _e)
]

binds @racket[x] to 1, @racket[y] to 2, and @racket[z] to 3 in
the scope of @racket[_e].

The syntax of a @racket[let*] expression allows any number of binders to occur,
so @racket[(let* () _e)] is valid syntax and is equivalent to @racket[_e].

Unlike @racket[let], @racketblock[(let* ((x 1) (y x)) 0)] is @emph{not} a
syntax error. However, bindings are only available forward, so
@racketblock[(let* ((x y) (y 1)) 0)] @emph{is} a syntax error.

HINT: Think about what a lazy compiler writer would do.
