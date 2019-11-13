#lang scribble/manual

@(require (for-label racket)
          "../notes/ev.rkt")

@title{Midterm 2}

@bold{Due: Thurs, Oct 14, 11:59PM}

@(define repo "https://classroom.github.com/a/bJm4_Ug6")

Midterm repository:
@centered{@link[repo repo]}

The repository contains a single markdown file @tt{m2.md}, which you
can edit to submit your answers to the following questions.  Your
submision must be pushed by midnight on Thursday.

During the 48 hours of the exam period, you may only ask private
questions on Piazza if you need assistance for the course staff.  You
may not comminicate or collaborate with any one else about the content
of this exam.

@section[#:tag-prefix "m2"]{Short answer}

@bold{Question 1}

[12 points]

Using only @racket[cons], @racket['()], symbols, and literal numbers,
strings, booleans, or characters, write expressions that are
equivalent to each of the following:

@itemlist[

@item{@verbatim{'((1) x 2)}}

@item{@verbatim{'((1) x . 2)}}

@item{@verbatim{'(1 . (2 . (3)))}}

@item{@verbatim{'(quote (x 2))}}

@item{@verbatim{`(1 ,(add1 2))}}

@item{@verbatim|{`(1 ,@'(2 3) x)}|}
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

Which of the following subexpressions are in tail position?

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

@section[#:tag-prefix "m2"]{Defunctionalizing}

@bold{Question 5}

[20 points]

Here is a program we wrote for computing the product of a binary tree
of numbers.

It is clever in that it never multiplies if there is a zero in the tree;
it immediately returns @racket[0].  It does this without using
exception handlers, but instead by being written in a
continuation-passing style.

@#reader scribble/comment-reader
(racketblock
;; BT -> Number
(define (prod bt)
  (prod/k bt (λ (x) x)))

;; BT (Number -> Number) -> Number
(define (prod/k bt k)
  (match bt
    ['leaf (k 1)]
    [`(node ,v ,l ,r)
     (if (zero? v)
         0
         (prod/k l (λ (pl)
                    (prod/k r (λ (pr)
                                (k (* v (* pl pr))))))))]))
)

Use defunctionalization to derive an equivalent definition of
@racket[prod] that does not use @racket[λ]-expressions or higher-order
values (i.e. no functions consume or produce other others).

The resulting program should consist of three mutually-recursive,
first-order functions where every call to one of these functions is a
tail-call.

For full credit, be sure to include a type definition for the type of
defunctionalized @racket[λ]-expressions in this program and type
signatures for all three functions.



@section[#:tag-prefix "m2"]{Pattern matching}

@bold{Question 6}

[30 points]

When we studied how to transform away pattern-matching expressions,
we considered the following grammar of patterns:

@#reader scribble/comment-reader
(racketblock
;; type Pat =
;; | #t
;; | #f
;; | Integer
;; | String
;; | Variable
;; | `_
;; | `'()
;; | `(quote ,Symbol)
;; | `(cons ,Pat ,Pat)
;; | `(list ,Pat ...)
;; | `(? ,Expr ,Pat ...)
 )

Let's make a modest extension:
@#reader scribble/comment-reader
(racketblock
;; type Pat =
;; ...
;; | (list 'list Pat '...)
 )

The pattern @racket[(list _p ...)] is a pattern that matches a list of
any length, so long as every element of the list matches the
subpattern @racket[_p].  Note that the elipsis here is literally part
of the syntax!

If the pattern matches, it matches any pattern variables within
@racket[_p] to the @emph{list} of things that match in the elements of
the list.

Some examples:

@ex[
(match (list 1 2 3)
  [(list xs ...) (reverse xs)])

(match (list 'x 'y 'z)
  [(list (? symbol? xs) ...) xs])

(match (list)
  [(list (? symbol? xs) ...) xs])

(match (list 'x 3 'z)
  [(list (? symbol? xs) ...) xs]
  [_ '()])

(match '((x 1) (y 2) (z 3))
  [(list (list xs ys) ...) (list xs ys)])
]


Extend the definitions of @racket[pat-match] and @racket[pat-bind]
which wrote when implementing pattern matching to include this new
kind of pattern.

@#reader scribble/comment-reader
(racketblock
;; Pat Variable -> Expr
;; Produces an expression determining if p matches v
(define (pat-match p v)
  (match p
    [(list 'list p1 '...)
     ;; your solution here
     'todo]
    ;; omitted code that was previously given
    ))

;; Pat Variable Expr -> Expr
;; Produce an expression that deconstructs v and binds pattern variables
;; of p in scope of e.
;; ASSUME: v matches p
(define (pat-bind p v e)
  (match p
    [(list 'list p1 '...)
     ;; your solution here
     'todo]
    ;; omitted code that was previously given
    ))
)

You do not have to transcribe the complete function, just give the
code that goes in two occurrences of @racket['todo] to complete the
definition.

If you need to rely on any helper functions, you must give their
complete definition and type signatures.

