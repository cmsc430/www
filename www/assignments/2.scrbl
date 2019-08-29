#lang scribble/manual
@title[#:tag "Assignment 2" #:style 'unnumbered]{Assignment 2: Racket Primer}

@bold{Due: Tues, Sept 10, 11:59PM}

@(define repo "https://classroom.github.com/a/KSlWPWTz")

The goal of this assignment is to gain practice programming in Racket.

Assignment repository:
@centered{@link[repo repo]}

You are given a repository with a @tt{main.rkt} file that contains a
number of sections.  In each section there are several function
``stubs,'' i.e. incomplete function defitions with type signatures,
descriptions, and a small set of tests.  Each function has a bogus
(but type correct) body marked with a ``TODO'' comment.  Your job is
to replace each of these expressions with a correct implementation of
the function.

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
  the same directory as @tt{main.rkt}.}

 @item{Pushing to github. The repository is set up to use
  Travis CI, a ``continuous integration'' system, to
  automatically run the test suite when a push occurs. You
  should get email notifications if tests fail. You can also
  see test reports at:
  @centered{@link["https://travis-ci.com/cmsc430/"]{
    https://travis-ci.com/cmsc430/}}

  (You will need to be signed in in order see results for your private repo.)}]

Note that running @tt{racket main.rkt} from the command line will not
run the tests.

@section[#:tag-prefix "a2-" #:style 'unnumbered]{Submitting}

Pushing your local repository to github ``submits'' your work.  We
will grade the latest submission that occurs before the deadline.

