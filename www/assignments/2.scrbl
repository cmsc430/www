#lang scribble/manual
@title[#:tag "Assignment 2" #:style 'unnumbered]{Assignment 2: Racket Primer}

@bold{Due: Tuesday, February 10, 11:59PM}

The goal of this assignment is to gain practice programming in Racket.

You are given a @tt{main.rkt} file (on ELMS under "Files"), that
contains a number of sections.  In each section there are several
function ``stubs,'' i.e. incomplete function definitions with type
signatures, descriptions, and a small set of tests.  Each function has
a bogus (but type correct) body marked with a ``TODO'' comment.  Your
job is to replace each of these expressions with a correct
implementation of the function.

The last section of problems deals with functions that operate over a
representation of expressions in a lambda-calculus-like language and
asks you to compute a few simple facts about the given expression.

Make sure you do not rename the file.  Also make sure not to change
the name or signature of any function given to you.  You may add any
additional functions that help you solve the overall problem you're
tackling.

@section[#:tag-prefix "a2-" #:style 'unnumbered]{Testing}

You can test your code in several ways:

@itemlist[

 @item{Running the code in DrRacket will (by default) run the
  test submodule and print out a report of any test failures.
  This is actually a configurable preference, but it is on by
  default.}

 @item{Using the command line @tt{raco test main.rkt} from
  the same directory as @tt{main.rkt}.}]

Note that running @tt{racket main.rkt} from the command line will
@bold{not} run the tests.

@section[#:tag-prefix "a2-" #:style 'unnumbered]{Submitting}

Submit your filled-in @tt{main.rkt} file on Gradescope.

@section[#:tag-prefix "a2-" #:style 'unnumbered]{Grading}

Your submission will be graded for correctness.  Passing the unit
tests included in the file is necessary but @bold{not sufficient} to
receive a perfect score.  You are strongly encouraged to add your own
tests to ensure the correctness of your solutions.