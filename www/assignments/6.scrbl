#lang scribble/manual
@title[#:tag "Assignment 6" #:style 'unnumbered]{Assignment 6: Syntax Checking}

@(require (for-label (except-in racket ...)))
@(require "../notes/ev.rkt"
          "../notes/utils.rkt")

@bold{Due: Tuesday, November 30th, 11:59PM EDT}

The goal of this assignment is to add syntax checking to our compiler.

You are given a repository with a starter compiler similar to the
@seclink["Mountebank"]{Mountebank} language we studied in class.  You
are tasked with:

@itemlist[

@item{implementing compile-time syntax checking.}

]

@section[#:tag-prefix "a6-" #:style 'unnumbered #:tag "checking"]{Syntax Checking}


Up until now, we've written our compiler assuming that programs are
well-formed, but there has never been any code that actually checks
this assumption.  The assumptions go beyond the properties checked
during parsing.  For example, our parser will happily accept a program
like @racket[(lambda (x x) x)], and the compiler will emit code for it
even though this is not a well-formed program.

The idea of this assignment is to implement a function, defined in
@tt{check-syntax.rkt}:

@#reader scribble/comment-reader
(racketblock
;; Prog -> Prog
(define (check-syntax p) ...)
)

If the program is well-formed, it should behave like the identity
function, returning the same program it was given as input.  On
ill-formed programs, it should signal an error using @racket[error].

For the purposes of this assignment, the quality of the error messages
doesn't matter, although in real programming language implementations,
good error messages are crucial.

Here are the properties that should be checked of each program:

@itemlist[

@item{Programs are @emph{closed}; there are no unbound variables in
the program.}

@item{Every @racket[define]d function should have a distinct name.}

@item{Every function parameter should be distinct from the other
parameters of that function.}

@item{Every function's name should be distinct from all of its
parameters' names.}

@item{Every function name and variable should not clash with any of
the keywords of our language, e.g. @racket[lambda], @racket[if], etc.
(Note this is not a restriction Racket puts on programs; the following
is a perfectly reasonable expression: @racket[(λ (λ) λ)], but we'll
consider this a syntax error.)}

@item{Every pattern variable in a pattern should be distinct.  (Racket
also allows this, but it has a complicated run-time semantics which we
don't implement so instead we just rule out these programs.)}

]

The starter code calls @racket[check-syntax] in both
@tt{compile-file.rkt} and @tt{interp-file.rkt}.  The definition of
@racket[check-syntax] is stubbed out in @tt{check-syntax.rkt}.

There are tests included in @tt{test/check-syntax.rkt}. You can also use this file 
to write your own tests for debugging.

@section[#:tag-prefix "a6-" #:style 'unnumbered]{Submitting}

You should submit on Gradescope. You should submit a zip file that has
exactly the same structure that the stub contains. We will only use
the @tt{check-syntax.rkt} files for grading, so make sure all your
work is contained there!
