#lang scribble/manual
@title[#:tag "Assignment 5" #:style 'unnumbered]{Assignment 5: A Heap of Characters}

@(require (for-label (except-in racket ...)))
@(require redex/pict)

@(require "../notes/ev.rkt")

@bold{Due: Thursday, April 7th, 11:59PM EDT}

@(define repo "https://github.com/cmsc430/assign05")

The goal of this assignment is to extend a compiler with data types
that require memory allocation and dereferencing. A secondary goal is to hone your test-writing skills.

Assignment repository:
@centered{@link[repo repo]}

You are given a repository with a starter compiler similar to the
@seclink["Hustle"]{Hustle} language we studied in class.  You are tasked
with:

@itemlist[

@item{extending the language to include a string data type,}

@item{extend the (very few) tests provided to be as comprehensive as possible.}

]


@section[#:tag-prefix "a5-" #:style 'unnumbered]{Strung out}

In Dodger, we implemented a character data type for
representing single letters.  In this assignment, you will implement a
String data type for representing arbitrarily long sequences of
characters.


Strings are disjoint from all other data types and are essentially a
fixed-size array of characters.  Literal strings are written by
enclosing the characters within the string in double quotes (@tt{"}).
Strings can include double quotes by using the escape sequence
@tt{\"}.

You must add the following operations to Hustle+:

@itemlist[
@item{@code[#:lang "racket"]{string? ; Any -> Boolean}, which determines if its argument is a string.}
@item{@code[#:lang "racket"]{string-ref ; String Natural -> Char}, which 
extracts the character at the given index (using 0-based counting).  An error is signalled if 
the index is out of bounds for the given string or if the first argument is not a string.}
@item{@code[#:lang "racket"]{string-length ; String -> Natural}, which computes the length of the string, or yields an error if its argument is not a string.}
@item{@code[#:lang "racket"]{make-string ; Natural Char -> Natural}, which constructs a string of the given
length, filled with the given character If the first argument is not a non-negative integer, of if the second argument is not a character, an error should be returned.}
]

We have already added a @tt{String} struct in @tt{ast.rkt} and
provided the parsing code in @tt{parse.rkt}.

More importantly, the run-time system has been updated to account for
a string type.  It assumes a representation where the length of the
string is stored in memory, followed by the characters of the string,
in order.  You can change the representation if you'd like,
but you will have to update the run-time system to properly print
strings and discuss it with the instructors beforehand. Not recommended!
Otherwise, no changes to the run-time system should be necessary.

If you want to understand the details of how strings are implemented in
the run-time system. See the function @tt{print_string()}
in @tt{main.c}.

In order to get all the points for this section of the assignment you will
need to modify the following files:

@itemlist[

@item{@tt{interp.rkt}}
@item{@tt{interp-prim.rkt}}
@item{@tt{compile.rkt}}

]

@section[#:tag-prefix "a5-" #:style 'unnumbered]{Add tests!}

One thing that has been not stressed enough this semester is the need
to add tests. There are 6 files under the @tt{test/} directory in the
repo. Three of them contain regression tests to ensure that you don't
break functionality when you add the new constructs. These are
@tt{test-runner.rkt}, @tt{interp.rkt} and @tt{compile.rkt}. The
@tt{test-runner.rkt} file provides two functions @tt{test-runner} and
@tt{test-runner-io} which execute a sequence of calls to an input
@tt{run} function, and check that they yield the expected result. This
@tt{run} function is instantiated in @tt{interp.rkt} with a call to
the interpreter, and in @tt{compile.rkt} with a call to the compiler.

With this setup, when you do @tt{raco test test/interp.rkt} or
@tt{raco test test/compile.rkt} you should be seeing "76 tests passed".

There are also three similar files with the "-string" suffix in the
test directory in the repo (@tt{interp-string.rkt},
@tt{compile-string.rkt}, and @tt{test-runner-string.rkt}.  You'll
notice that there are exactly two public tests there - one using io
and one not. The second part of the assingment is to extend these tests
to thoroughly test the behavior of the four primitives you added.

You should ONLY add tests to @tt{test-runner-string.rkt} and they should
all be of the same form as the ones provided: either @tt{(check-equal? (run XXX) Y)}
for the @tt{test-runner} function, or @tt{(check-equal? (run XXX SSS) (cons YYY ZZZ))}
if your tests use IO.

Your test suite will be autograded based on its ability to reveal bugs
in a series of implementations we have provided, each with 1 or more
bugs injected. That is, we will run @tt{raco test} using YOUR
@tt{test-runner-string} file against OUR (intentionally faulty) 
implementations. Of course, your test suite should not be failing 
any tests for a correct implementation.

@section[#:tag-prefix "a5-" #:style 'unnumbered]{Submitting}

You should submit on Gradescope. You should submit a zip file that has
exactly the same structure that the stub contains. We will only use
the @tt{compile.rkt}, @tt{interp.rkt}, @tt{interp-prim.rkt}, and
@tt{test/test-runner-string.rkt} files for grading, so make sure all
your work is contained there! 
