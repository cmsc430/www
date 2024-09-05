#lang scribble/manual
@title[#:tag "Assignment 2" #:style 'unnumbered]{Assignment 2: a86 Primer}

@bold{Due: Thursday, September 12, 11:59PM}

The goal of this assignment is to gain practice programming in a86.

@bold{This is a collaborative assignment.}  You may work with anyone
you'd like on this assignment, but each person must submit their
@tt{submit.zip} file on Gradescope.

You are given a @tt{a86-basics.zip} file (on ELMS under "Files"), that
contains a README, a Makefile, and a number of Racket modules.  In
each module there are several ``stubs,'' i.e. incomplete definitions
with type signatures, descriptions, and a small set of tests.  Each
definition has a bogus (but type correct) body marked with a ``TODO''
comment.  Your job is to replace each of these expressions with a
correct implementation of the a86 code.

Make sure you do not rename any files.  Also make sure not to change
the name or signature of any definition given to you.  You may add any
additional functions that help you solve the overall problem you're
tackling.

@section[#:tag-prefix "a2-" #:style 'unnumbered]{Testing}

You can test your code in several ways:

@itemlist[

 @item{Running the code in DrRacket will (by default) run the
  test submodule and print out a report of any test failures.
  This is actually a configurable preference, but it is on by
  default.}

 @item{Using the command line @tt{raco test <filename.rkt>} from
  the same directory as your Racket code will test the module
  in @tt{<filename.rkt>}.  If you run @tt{raco test .}, it will
  test all of the Racket files in the current directory.}]

Note: running @tt{racket <filename.rkt>} will @bold{not} test the
file; you need to use @tt{raco} or DrRacket.

@section[#:tag-prefix "a2-" #:style 'unnumbered]{Submitting}

Use the included Makefile to run @tt{make submit.zip} (or simply
@tt{make}) to generate an appropriate @tt{submit.zip} file for
submitting to Gradescope.

@section[#:tag-prefix "a2-" #:style 'unnumbered]{Grading}

Your submission will be graded for correctness.  Passing the unit
tests included in the file is necessary but @bold{not sufficient} to
receive a perfect score.  You are strongly encouraged to add your own
tests to ensure the correctness of your solutions.
