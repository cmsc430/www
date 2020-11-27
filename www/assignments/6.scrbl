#lang scribble/manual
@title[#:tag "Assignment 6" #:style 'unnumbered]{Assignment 6: Apply, arity checking, and variable arity functions}

@(require (for-label (except-in racket ...)))
@(require redex/pict)

@(require "../notes/ev.rkt")

@bold{Due: Thursday, December 10th, 11:59PM EST}

@(define repo "https://FIXME")

The goal of this assignment is to (1) implement arity checking in a
language with functions, (2) to implement the @racket[apply]
operation, a fundamental operation that allows functions to be applied
to lists of arguments, and (3) to implement variable arity functions,
i.e. functions that take any number of arguments.

Assignment repository:
@centered{@link[repo repo]}

You are given a repository with a starter compiler similar to the
@seclink["Iniquity"]{Iniquity} language we studied in class.

This started code is slightly different from past code in that:

@itemlist[

@item{It implements all of the ``plus'' features we've covered in
previous assignments.  It is worth studying this code to compare with
the solutions you came up with.  Try to formulate a technical critique
of both solutions.}

@item{Parsing has been eliminated.  Instead the code uses
@racket[read] to read in an S-Expression and syntax checking is done
at the level of S-Expressions.  Hopefully you've noticed that as the
langauge gets larger and more complicated, so too does the parsing.
Writing the parsing code is both tedious and easy to get wrong.  Why
is this?  Our data representation of @tt{Expr}s is a subset of
@tt{S-Expression}s (the stuff you can write with @racket[quote]).
We're trying to process an unstructured stream of tokens into a
structured, nested datum, but also at the same time verify that datum
follows the rules of @tt{Expr}ness.

This design can be simplified by first processing tokens into
@tt{S-Expression}s, @emph{then} checking whether this
@tt{S-Expression} is in the @tt{Expr} subset.  Parsing the whole
@tt{S-Expression} set is actually easier than parsing the @tt{Expr}
subset.  And we can use an existing tool for doing it: @racket[read].

This was in fact our first approach to parsing.  We have returned from
this detour so that hopefully you can appreciate the value in it's
approach.  Concrete syntax and parsing are orthogonal to the semantics
of a language, which is the real focus of a compiler.}

]

@section[#:tag-prefix "a6-" #:style 'unnumbered]{Overview}

Of all the assignments so far, this one asks you to write the least
amount of code, and yet, is probably the most difficult.


You are free to tackle this parts in any order you see fit, @emph{but}
I recommend approaching them in the order described here so that you
can peicemeal work up to a full solution.  Tackling everything at once
will make things harder.


@section[#:tag-prefix "a6-" #:style 'unnumbered]{Apply yourself}

The @racket[apply] operation gives programmers the ability to apply a
function to a list as though the elements of that list are the
arguments of the function.  In other words, it does the following:
@racket[(apply f ls)] @math{=} @racket[(f v1 ...)], where @racket[ls]
is @racket[(list v1 ...)].

Your task is to implement (a slightly simplified version) of the
@racket[apply] operation, which takes the name of a function (this
assignment is based on @seclink["Iniquity"]{Iniquity}, which only has
second-class functions) and an argument that is expected to evaluate
to a list.  The given function is then called with the elements of the
list as its arguments.

For example, this calls the @racket[f] function, which expects two
arguments and adds them together, with a list containing @racket[1]
and @racket[2].  The result is equivalent to @racket[(f 1 2)],
i.e. @racket[3]:

@ex[
(begin
  (define (f x y) (+ x y))
  (apply f (cons 1 (cons 2 '()))))
]

Note that the list argument to @racket[apply] is an arbitrary
expression, which when evaluated, should produce a list.  We could
have written the example as follows to emphasize that the compiler
cannot simply examine the sub-expression to determine the arguments to
the function being called:

@ex[
(begin
  (define (f x y) (+ x y))
  (define (g z) (cons z (cons 2 '())))
  (apply f (g 1)))
]

This addition adds the following to @tt{Expr}s:


@#reader scribble/comment-reader
(racketblock
;; type Expr =
;; ...
;; | `(apply ,Variable ,Expr)
)

The @tt{syntax.rkt} file contains code for checking the syntax of Iniquity+.

The @tt{interp.rkt} file contains a reference implementation of
@racket[apply].

The @tt{compile.rkt} file contains the compile for Iniquity+, and has
stubbed out a function for compile @racket[apply] expressions:

@#reader scribble/comment-reader
(racketblock
;; Variable Expr CEnv -> Asm
(define (compile-apply f e c) '())
)

Your job is to correctly implement this function.

The key idea here is that evaluating @racket[e] should produce a list.
That list is going to be represented as a linked list of pair pointers
in the heap that eventually terminates in the empty list.  But in
order to call a function, we need to have arguments placed
appropriately on the stack.

@margin-note{The compiler will need to emit code to check the
assumption that the argumgent is a list and signal an error if it is
violated, but you might first try to get this working on examples that
are correct uses of @racket[apply] before thinking about the error
cases.}

Until now, the compiler has always known how many arguments are given:
you just look at the syntax of the call expression.  But here we don't
know statically how many elements of the list there are.

So, the compiler will need to emit code that traverses the list and
places the element at the appropriate position on the stack.  After
this, the function can be called as usual.

Since the list can be arbitrarily large, the emitted ``copy list
elements to stack'' code will need to be a loop.  Here's a sketch of
how the loop operates:

@itemlist[

@item{it has a register containing the list (either
empty or a pointer to a pair).}

@item{it has a register containing a pointer to the
next spot on the stack to put an argument.}

@item{if the list is empty, the loop is done.}

@item{if the list is a pair, copy the @racket[car] to the stack, set
the register holding the list to @racket[cdr], and advance the
register holding the pointer to the stack.}

]

Once you have this working for ``good examples,'' revisit the code to
interleave type checking to validate that the subexpression produced a
list.


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

The challenge here is that the arity needs to be checked at run-time
(at least it will be with the addition of first-class functions,
or... @racket[apply]).  But at run-time, we don't have access to the
syntax of the function definition or the call.  So in order to check
the arity of a call, we must emit code to do the checking and to
compute the relevant information for carrying out the check.

Here is the idea: when compiling a function definition, the arity of
the function is clear from the number of parameters of the definition.
If the caller of a function can communicate the number of arguments to
the function, then the function can just check that this number
matches the expected number.

When compiling a call, the number of arguments is obvious, so the call
should communicate this number to the function (which then checks it).

How should this number be communicated?  A simple solution is to pass
the number as though it were the first argument of the function.

Hence, a function of @math{n} arguments will be compiled as a function
of @math{n+1} arguments.  A call with @math{m} arguments will be
compiled as a call with @math{m+1} arguments, where the value of the
first argument is @math{m}.  The emitted code for a function should
now check that the value of the first argument is equal to @math{n}
and signal an error when it is not.

You will need to modify @racket[compile-call] and
@racket[compile-define] to implement this arity checking protocol.
You will also need to update @racket[compile-apply], but first try to
get things working for normal calls.


@section[#:tag-prefix "a6-" #:style 'unnumbered]{Apply with arity checks}

What happens now with your previously good examples of using
@racket[apply]?  Why is everything coming up @racket['err] now?

The problem is that once you implement the arity checking mechanism
for function definitions, this checking will also happen when you call
the function via @racket[apply], but your @racket[apply] code is not
communicating the number of arguments, so the call is likely to fail
the check.  (Pop quiz: can you make an example that ``works,''
i.e. calls a function via @racket[apply] but avoids the arity check
error?)

In order to get @racket[apply] working again, you'll need to have it
follow the protocol for calling the function with the number of
arguments as the first argument.

But how many arguments are there?  Well, there are as many arguments
as there are elements in the list.  Update @racket[compile-apply] so
the emitted code computes this quantity and passes it in the
appropriate spot on the stack.  After doing this, all your earlier
examples should work again, it should catch arity errors where the
function expects a different number of arguments from the number of
elements in the list.

@section[#:tag-prefix "a6-" #:style 'unnumbered]{Variable arity functions}

So far, the arity of every function is some fixed natural number.
However, it's quite useful to have functions that can take any number
of arguments.

This is possible in Racket with the use of variable arity functions.

For example:

@ex[
(begin
  (define (f . xs) xs)
  (f 1 2 3))
]

Note the use of ``@tt{.}'' in the formal parameters of the function.
This syntax is indicating that @racket[f] can be applied to any number
of arguments (including 0).  The arguments will be bundled together in
a list, which is bound to the @racket[xs] variable.  So this
application produces the list @racket['(1 2 3)].

We can also write a function like this:

@ex[
(begin
  (define (f x y . zs) zs)
  (f 1 2 3))
]

This function must be applied to at least 2 arguments.  The first two
arguments are bound to @racket[x] and @racket[y].  Any additional
arguments are bundled in a list and bound to @racket[zs].  So this
expression produces a list of one element: @racket[3].

To accomodate this, the syntax of formal parameters in function
definitions is updated from:

@#reader scribble/comment-reader
(racketblock
;; type Formals = (Listof Variable)
)

To:
@#reader scribble/comment-reader
(racketblock
;; type Formals =
;; | '()
;; | Variable
;; | (Cons Variable Formals)
)

Meaning the formals ``list'' can be an improper list and when it is,
the final variable is the one that binds to a list containing all
remaining arguments.

To implement variable arity functions, you'll need to update
@racket[compile-define] to handle this form of function defintion.
The code includes a stubbed case for matching variable arity function
definitions; you just need to write the code.

The idea is inverse to that of @racket[apply]: an arbitrary number of
arguments are passed on the stack and the function must convert the
appropriate number of stack arguments to a list.

You'll need to implement this part of the assignment after adding the
arity checking protocol.  This is because the function caller needs to
pass in the count of the number of arguments.  Without this
information, the function won't know how many arguments to put in a
list.

If the function requires at least @math{n} arguments and is called
with @math{m} arguments, then @math{m-n} arguments should be placed in
a list and the list should be passed in as the @math{n+1}th argument.
(If the function is called with less than @math{n} arguments, then it
is an arity error.)  So like @racket[apply], you'll need a loop, but
instead of copy from a list to a stack, you'll need to copy from the
stack to a list.

@section[#:tag-prefix "a6-" #:style 'unnumbered]{Bonus}

Should you find yourself having completed the assignment with time to
spare, you could try adding proper tail calls to the compiler.  You
will have to make it work with arity checking and calling
@racket[apply] in tail position should make a tail call to the
function.

This isn't worth any credit, but you might learn something.


@section[#:tag-prefix "a6-" #:style 'unnumbered]{Testing}

You can test your code in several ways:

@itemlist[

 @item{Using the command line @tt{raco test .} from
  the directory containing the repository to test everything.}

 @item{Using the command line @tt{raco test <file>} to
  test only @tt{<file>}.}

 @item{Pushing to github. You can 
  see test reports at:
  @centered{@link["https://travis-ci.com/cmsc430/"]{
    https://travis-ci.com/cmsc430/}}

  (You will need to be signed in in order see results for your private repo.)}]

Note that only a small number of tests are given to you, so you should
write additional test cases.

@bold{There is separate a repository for tests!} When you push your
code, Travis will automatically run your code against the tests.  If
you would like to run the tests locally, clone the following
repository into the directory that contains your compiler and run
@tt{raco test .} to test everything:

@centered{@tt{https://github.com/cmsc430/assign06-test.git}}

This repository will evolve as the week goes on, but any time there's
a significant update it will be announced on Piazza.

@section[#:tag-prefix "a6-" #:style 'unnumbered]{Submitting}

Pushing your local repository to github ``submits'' your work.  We
will grade the latest submission that occurs before the deadline.
