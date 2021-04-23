#lang scribble/manual
@title[#:tag "Assignment 6" #:style 'unnumbered]{Assignment 6: Arities!}

@(require (for-label (except-in racket ...)))
@(require redex/pict)

@(require "../notes/ev.rkt")

@bold{Due: Thursday, April 29th, 11:59PM EST}

@(define repo "https://github.com/cmsc430/assign06")

The goal of this assignment is to implement arity checking in a
language with functions, and (2) to implement the @racket[procedure-arity]
operation for accessing the arity of a function.

Assignment repository:
@centered{@link[repo repo]}

You are given a repository with a starter compiler similar to the
@seclink["Loot"]{Loot} language we studied in class. The only change
has been the addition of parsing code for the unary
@racket[procedure-arity] primitive.

@section[#:tag-prefix "a6-" #:style 'unnumbered]{Arity-check yourself, before you wreck yourself}

When we started looking at functions and function applications, we
wrote an interpreter that did arity checking, i.e. just before making
a function call, it confirmed that the function definition had as many
parameters as the call had arguments.

The compiler, however, does no such checking.  This means that
arguments will silently get dropped when too many are supplied and
(much worse!) parameters will be bound to junk values when too few are
supplied; the latter has the very unfortunate effect of possibly
leaking local variable's values to expressions out of the scope of
those variables.  (This has important security ramifications.)

The challenge here is that the arity needs to be checked at run-time,
since we have first class functions. But at run-time, we don't have
access to the syntax of the function definition or the call.  So in
order to check the arity of a call, we must emit code to do the
checking and to compute the relevant information for carrying out the
check.

The main high-level idea is that: when compiling a function
definition, the arity of the function is clear from the number of
parameters of the definition; when compiling a call, the number of
arguments is also obvious. Therefore, what's needed is a way for the
the function and the call to communicate and check their corresponding
arity information.

We recommend storing the arity of the function as an additional piece
of information in the closure during its compilation. Then, during a
call you can access that arity and check it before making the call.
Bonus: it makes implementing @racket[procedure-arity] really
straightforward: you just have to access that number.

Just like we've been saying all semester, there are multiple other
ways of going about this, feel free to design and implement a solution
that works correctly - and consider the trade-offs! For example,
another approach would be to treat the arity of the function as if it
were the first argument of the function. A function of @math{n}
arguments would then be compiled as a function of @math{n+1}
arguments.  A call with @math{m} arguments would be compiled as a call
with @math{m+1} arguments, where the value of the first argument is
@math{m}.  The emitted code for a function should then check that the
value of the first argument is equal to @math{n} and signal an error
when it is not. But how would you implement @racket[procedure-arity]
in this case? (This is not a rhetorical question, if you have a
realistic solution to this, send us an e-mail!)

Your job is to modify @racket[compile.rkt] and to implement this arity
checking protocol and the @racket[procedure-arity] primitive. It might
help to implement the primitive before compiling the calls themselves,
to partially test your implementation. Unlike previous assignments,
there are no explicitly marked TODOs (with the exception of
@racket[procedure-arity]). You have to make sure you modify all places
where closures are created/accessed to ensure that your changes work
correctly!

As always, remember to test your code using both the testcases
provided and by adding your own!

@section[#:tag-prefix "a6-" #:style 'unnumbered]{Submitting}

Submit just the @tt{compile.rkt} file on Gradescope.