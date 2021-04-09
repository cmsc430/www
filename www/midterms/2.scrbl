#lang scribble/manual

@(require (for-label racket)
          "../notes/ev.rkt")

@title{Midterm 2}

@bold{Due: Tuesday, April 13th 11:59PM}

@(define repo "https://github.com/cmsc430/Midterm2-prog")

Midterm repository:
@centered{@link[repo repo]}

The exam consists of two parts: a written portion and a programmatic
portion.  Both will be handled through gradescope. You will see two
gradescope assignments marked accordingly.

During the exam period, you may only ask private questions to
the staff (via email, discord, etc.) if you need clarification.
You may not communicate or collaborate with any one else about the
content of this exam.

Questions that are deemed applicable to the entire class will be shared, along
with their responses, with the rest of the class.

The repository contains two things.
@itemlist[
@item{A folder @tt{BoxIncr} which contains the base code to build upon for Question 4.}
@item{A folder @tt{CallByName} which contains the base code to build upon for Question 5.}
]

Your submission must be submitted by 11:59 EDT on Tuesday, April
13th. For the programmatic fragment, you should submit a zip file
containing two files: the @tt{BoxIncr/compile.rkt} for 
Question 4, and @tt{CallByName/interp.rkt} for Question 5.

@section[#:tag-prefix "m2"]{Short answer}

@bold{Question 1}

[10 points]

@itemlist[

@item{On the random generation lecture on Tuesday (a recording can be
found on ELMS!), we wrote random generators for testing the @tt{Con}
and @tt{Dupe} languages. When going from @tt{Con} to @tt{Dupe},
we modified the generators to take an additional argument representing
the type of the expression we want to generate. Why?}

@item{Why did we need to introduce the @tt{Lea} assembly
instruction when implementing @tt{Knock}?}
]

@bold{Question 2}

[10 points]

Is it possible for @tt{fib1} to run out of memory? Justify your
answer.

@#reader scribble/comment-reader
(racketblock
(define (fib1 i)
  (match i
    [0 1]
    [1 1]
    [_ (+ (fib1 (- i 1)) (fib1 (- i 2)))]))
)


How about @tt{fib2}?  Again, justify your answer.

@#reader scribble/comment-reader
(racketblock
(define (fib2-aux i fib-1 fib-2)
  (if (zero? i) fib-1
      (fib2-aux (sub1 i) (+ fib-1 fib-2) fib-1)))

(define (fib2 i)
  (match i
    [0 1]
    [1 1]
    [_ (fib2-aux (sub1 i) 1 1)]))
)

Hint: consider Jig.

@bold{Question 3}

[10 points]

For each of the following expressions, which subexpressions are in tail
position? Assume that the top-level expression is in tail position.

@itemlist[

@item{@verbatim{(sub1 e0)}}

@item{@verbatim{(begin e0 e1)}}

@item{@verbatim{(let ((x a)) e)}}

@item{@verbatim{(if b (box e1) e2)}}

@item{@verbatim{(match e [p1 e1] [_ e2])}}

]

@section[#:tag-prefix "m2"]{Code generation}

@bold{Question 4}

[25 points]

In the repo (@link[repo repo]), you will find a directory named
"BoxIncr".  That contains the @tt{Hustle} language from the lectures,
partially extended with two additional primitives: @racket[incr-box!]
and an @racket[decr-box!].

An @racket[(incr-box! _e)] expression evaluates @racket[_e]. The
result of @racket[_e] should be a boxed integer (otherwise an error is
signalled).  The box is updated (mutated) to increment its value by 1.
Similarly, @racket[(decr-box! _e)] should decrement the boxed integer
by 1. The result of the operation should be @racket[void].

Here's an example that returns 42 (note: @racket[let] is used for
sequencing here):

@#reader scribble/comment-reader
(racketblock
(let ((b (box 41)))
  (let ((v (incr-box! b)))
    (unbox b)))
)

The ast, parser, and interpreter have already been updated for you to
implement this functionality. Your job is to implement the compiler.

@section[#:tag-prefix "m2"]{Call by Name}

@bold{Question 5}

[45 points]

In the @link[repo repo], you will find a stripped down version of the
@secref["Iniquity"] language: just the interpreter. Iniquity
introduces the notion of function definitions and function calls.  The
way we evaluate function calls (as in racket) is known as
"call-by-value": the arguments to a function call are evaluated
before the body of the function is evaluated.

An alternative evaluation strategy is "call-by-name". In call-by-name,
the arguments to a function call are substituted in the function body,
left to be evaluated as they appear: if an argument is unused, it will
never be evaluated; if an argument is used multiple times, it will be
evaluated multiple times.

Consider the following example:

@#reader scribble/comment-reader
(racketblock
(begin
 (define (f x) 42)
 (f (read-byte)))
)

In standard call-by-value, this will read a byte from standard input,
and proceed to call @tt{f} with that read value, ignore it, and return
42. In call-by-name, this program will not read any value from the
standard input - the @tt{read-byte} will never be evaluated, as the body
of @tt{f} does not make use of @tt{x}.

On the other hand, consider the following program:

@#reader scribble/comment-reader
(racketblock
(begin
 (define (f x) (+ x x))
 (f (read-byte)))
)

Again, in standard call-by-value, this will read a byte from standard
input, and proceed to call @tt{f} with that read value, doubling it.
In call-by-name, this program will result to two different calls to
@tt{read-byte}, whose results will then be added together: when
calling @tt{(f (read-byte))}, the argument will be substituted into
the body of @tt{f}, yielding @tt{(+ (read-byte) (read-byte))}.

An interesting point is how call-by-value interacts with
let-bindings. For this assignment, we will keep let-bindings strict,
just like in the current interpreter, that is the argument to the let
will be evaluated before the body is executed.

To fully understand the interactions between let and function calls, consider the following examples:

@#reader scribble/comment-reader
(racketblock
(begin
 (define (f x y) (+ (+ x x) (+ y y)))
 (let ((z (read-byte))) (f z (read-byte))))
)

This program has the effect of first evaluating @tt{read-byte} (for
example, say 42), and then substituting its value for @tt{z} in the
body of the let. Then it will evaluate the call @tt{(f z
(read-byte))}. This will have the effect of substituting the value of
@tt{z} (which has already been evaluated), and the expression
@tt{(read-byte)} for @tt{x} and @tt{y} in the body of @tt{f}. In turn,
that means that there will be two new calls to @tt{read-byte}, for
each occurence of @tt{y} in the body of f.

@#reader scribble/comment-reader
(racketblock
(begin
 (define (f x) (+ x y))
 (let ((y 42)) (f y)))
)

This program should yield an error - we're still using static scoping!

@#reader scribble/comment-reader
(racketblock
(begin
 (define (f x) (let ((y 42)) x))
 (let ((y 17)) (f y)))
)

This program should yield 17, in both evaluation methods.

@#reader scribble/comment-reader
(racketblock
(begin
 (define (f x) (let ((x 42)) x))
 (f 17))
)

This example should yield 42, the let in the body of f shadows the
argument. 

@#reader scribble/comment-reader
(racketblock
(begin
 (define (loop x) (loop x))
 (define (f x) 42)
 (f (loop 0)))
)

In call-by-name, this program terminates as the argument
@tt{(loop 0)} is never evaluated!

@#reader scribble/comment-reader
(racketblock
(begin
 (define (f x) 42)
 (f (+ #t #f)))
)

In call-by-name, this program also terminates: the error-producing
argument @tt{(+ #t #f)} is again never evaluated!

Your task is to modify the interpreter to implement this style of
evaluation.  The easiest way to do that is to re-implement
let-bindings and function calls using substitution instead of the
environment to pass arguments. While an implementation using
environments is possible, it can be very tricky to get right; you have
been warned!


@bold{Question Call-by-need: Extra Credit}

[20 points]

An alternative to call-by-name is call-by-need. In call-by-need, the
arguments to a function are not evaluated at call time (just like in
call-by-name), but the evaluation of the argument is memoized - it's
going to only be evaluated the first time it's needed and future
occurences will reuse that evaluation's result.

You're on your own on this one and there will be no autograder, so
reach out to set out a time to talk about your solution if you try to
tackle this!

@subsection{Submission and Grading}

We will only use two files for grading: @tt{BoxIncr/compile.rkt} and
@tt{CallByName/interp.rkt}. You should be able to submit a zip from
inside the cloned repo to Gradescope, but we will only be using these
two files for grading, so restrict your work in those.