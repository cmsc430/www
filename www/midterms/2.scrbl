#lang scribble/manual

@(require (for-label racket)
          "../notes/ev.rkt")

@title{Midterm 2}

@bold{Due: Sat, May 2, 21:00PM}

@(define repo "https://classroom.github.com/a/z6YwuQrz")

Midterm repository:
@centered{@link[repo repo]}

The repository contains a single markdown file @tt{m2.md}, which you can edit
to submit your answers to the following questions.  Your submission must be
pushed by 9pm on Saturday (unless you have already arranged otherwise).

During the 60 hours of the exam period, you may only ask private
questions on Piazza if you need assistance for the course staff.  You
may not communicate or collaborate with any one else about the content
of this exam.

@section[#:tag-prefix "m2"]{Short answer}

@bold{Question 1}

[12 points]

Using only @racket[cons], @racket['()], symbols, and literal numbers,
strings, booleans, or characters, write expressions that are
equivalent to each of the following:

@itemlist[

@item{@verbatim{'((10) y y z 2)}}

@item{@verbatim{'((2) . 2)}}

@item{@verbatim{'(a . (b . (c)))}}

@item{@verbatim{'(quote (x 2))}}

@item{@verbatim{`(1 . ,(add1 2))}}

@item{@verbatim|{`(1 ,@'(2 3) ,@'(x))}|}
]

For example, @racket['(1 2 3)] is equivalent to @racket[(cons 1 (cons 2 (cons 3 '())))].

@bold{Question 2}

[10 points]

Will the following program run forever using only a constant amount of
memory, or will it eventually consume all memory and crash?  Briefly
justify your answer.

@#reader scribble/comment-reader
(racketblock
(begin (define (flip x)
         (flop (add1 x)))
       (define (flop y)
         (flip (sub1 y)))
       (flip 5)) 
)

How about this one?  Again, justify your answer.

@#reader scribble/comment-reader
(racketblock
(begin (define (flip x)
         (flip (flop (add1 x))))
       (define (flop y)
         (flop (flip (sub1 y))))
       (flip 5)) 
)


@bold{Question 3}

[8 points]

For each of the following expressions, which subexpressions are in tail
position?

@itemlist[

@item{@verbatim{(if e0 e1 e2)}}

@item{@verbatim{(match e0 [p1 e1] [p2 e2])}}

@item{@verbatim{((λ (x y) e0) e1 e2)}}

@item{@verbatim{(eq? e0 e1)}}
          
]


@section[#:tag-prefix "m2"]{Code generation}

@bold{Question 4}

[20 points]

Suppose we wanted to add a @racket[set-box!] operation to our language.  

A @racket[(set-box! _e0 _e1)] expression evaluates @racket[_e0] and
@racket[_e1].  The result of @racket[_e0] should be a box (otherwise
an error is signalled).  The box is updated (mutated) to contain the
value of @racket[_e1].  Racket's @racket[set-box!] returns a special
value called @racket[void], but your compiler may have the expression
return any value.

Here's an example (note: @racket[let] is used for sequencing here):

@ex[
(let ((b (box 10)))
  (let ((i1 (set-box! b 2)))
    (unbox b)))
]

Implement the following function in the compiler.  (You may assume
this code will be added to the Loot compiler and may use any functions
given to you.)

@#reader scribble/comment-reader
(racketblock
;; LExpr LExpr CEnv -> Asm
;; Compiler for (set-box! e0 e1)
(define (compile-set-box! e0 e1 c) ...)
)

@section[#:tag-prefix "m2"]{Closure Conversion}

@bold{Question 5}

[20 points]


In our @secref["Loot"] compiler we used Closure Conversion to
help us implement higher-order functions on a target language
that does not support higher-order functions natively (x86_64).

This was done by introducing an appropriate data structure
for storing the environment and accessing the stored environment
when necessary.

Now imagine that our target was not x86_64, but a dialect of Racket
that did not have higher-order functions. We could still perform
closure conversion!

For example, the following application of a constant function:

@#reader scribble/comment-reader
(racketblock
(let ((x 5)) ((lambda (y) x) 10))
)

Using the same definitions for @tt{lookup} and @tt{ext} first introduced in
@secref["Fraud"], could be transformed to the following:

@#reader scribble/comment-reader
(racketblock

(define (lam1 env y)
  (lookup env 'x))

(let ((x 5)) (lam1 (ext '() 'x x) 10))
)

Perform closure conversion on the following higher-order program:

@#reader scribble/comment-reader
(racketblock
(let ((x 10)
      (y 20)
      (z 30))
 ((lambda (z) (+ x ((lambda (x) (+ x y)) y) z)) 1))
)

You may assume @tt{lookup} and @tt{ext} exist.

In order to receive full marks, show the steps of your transformation.

Clarification: I've completely removed @tt{map} as it was only essential to the
extra credit below. To get full marks you must closure convert both of the
lambdas that are present in the program above.


@bold{Question 5 Extra Credit}

[10 points]

Apply the full defunctionalization transformation introduced in @secref["Loot"]
to the following code:

@#reader scribble/comment-reader
(racketblock
(define (map f xs)
  (match xs
    ['() '()]
    [(cons y ys) (cons (f y) (map f ys))]))

(define (main)
  (let ((x 10)
        (y 20)
        (z 30))
   (map (lambda (z) (+ x ((lambda (x) (+ x y)) y) z)) '(1 2 3))))
)

You can reuse whatever you feel is appropriate, if anything, from Question 5.
