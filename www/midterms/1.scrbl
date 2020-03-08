#lang scribble/manual

@(require (for-label racket))

@title{Midterm 1}

@bold{Due: Tues, March 10th, 11:59PM}

@(define repo "https://classroom.github.com/a/eTxvTYHF")

Midterm repository:
@centered{@link[repo repo]}

The repository contains a single plaintext file @tt{m1.txt}, which you
can edit to submit your answers to the following questions.  Your
submission must be pushed by midnight on Tuesday.

During the 72 hours of the exam period, you may only ask private
questions on Piazza if you need assistance for the course staff.  You
may not communicate or collaborate with any one else about the content
of this exam.

@section{Short answer}

@bold{Question 1}

[10 points]

Assuming that the value in @tt{rax} is 5 and the value in @tt{rbx} is 10,
briefly describe the difference, if any, between these two Asm instructions:

@verbatim{
(mov rax rbx)
}

@verbatim{
(mov rax (offset rbx 0))
}

@bold{Question 2}

[10 points]

Briefly describe the difference, if any, between these two Asm instructions
(hint: offsets were first introduced in @tt{Fraud}) :

@verbatim{
(mov rax (offset rbx 1))
}

@verbatim{
(mov (offset rbx 1) rax)
}

@section{Representation}

@bold{Question 3}

[20 points]

When studying the @secref{Dupe} language, we used the least
significant bit of a 64-bit integer to indicate whether the value
being represented was an integer (tagged with @code[#:lang
"racket"]{#b0}) or a boolean (tagged with @code[#:lang
"racket"]{#b1}).

Consider the following alternative design: @racket[#t] is represented
by the number 1, @racket[#f] is represented by the number 0. All other
numbers beside 0 and 1 are used to represent integers.

@itemlist[

@item{Describe one way in which this design is worse that the tagging
approach we have used in class.}

@item{Describe on way in which this design is better than the tagging approach
used in class.}

@item{Describe, at a high-level in English, how you could implement
@racket[add1] using this design.}

]

@section{Interpreting Boolean connectives}

@bold{Question 4}

[25 points]

Consider the following interpreter from @secref{Extort}.

@#reader scribble/comment-reader
(racketblock 
;; type Answer = Value | 'err
 
;; Expr -> Answer
(define (interp e)
  (match e
    [(? integer? i) i]
    [(? boolean? b) b]
    [`(add1 ,e0)
     (match (interp e0)
       [(? integer? i) (add1 i)]
       [_ 'err])]
    [`(sub1 ,e0)
     (match (interp e0)
       [(? integer? i) (sub1 i)]
       [_ 'err])]
    [`(zero? ,e0)
     (match (interp e0)
       [(? integer? i) (zero? i)]
       [_ 'err])]
    [`(if ,e0 ,e1 ,e2)
     (match (interp e0)
       ['err 'err]
       [v
        (if v
            (interp e1)
            (interp e2))])]))

)

Now extend the interpreter to include @racket[and] and @racket[or]
connectives similar to those found in Racket.

The @racket[or] form takes any number of subexpressions.  The
subexpressions are evaluated from left to right until a subexpression
evaluates to a non-@racket[#f] value, which is produced by the
@racket[or].  If no such subexpression exists, then @racket[#f] is
produced.

The @racket[and] form is similar.  It takes any number of
subexpressions.  The subexpressions are evaluated from left to right
until a subexpression evaluates to a @racket[#f] value, which is
produced by @racket[and].  Otherwise, @racket[and] produces the value
of the last subexpression.  If there are no subexpressions, then
@racket[#t] is produced.

To make things interesting, you should not use Racket's @racket[and]
and @racket[or] in @racket[interp].

@section{Compiling a new primitive operation}

@bold{Question 5}

[35 points]

Consider the following excerpt of a compiler that is able to compile
the @racket[cons] primitive and empty lists @racket['()].  The
relevant representation information is given as is the function for
compiling @racket[cons]:

@#reader scribble/comment-reader
(racketblock

(define result-shift     3)
(define result-type-mask (sub1 (arithmetic-shift 1 result-shift)))
(define type-imm         #b000)
(define type-pair        #b010)

(define imm-shift        (+ 2 result-shift))
(define imm-type-empty   (arithmetic-shift #b11 result-shift))

;; Expr Expr CEnv -> Asm
(define (compile-cons e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c))))
    `(,@c0
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c1
      (mov (offset rdi 0) rax)
      (mov rax (offset rsp ,(- (add1 (length c)))))
      (mov (offset rdi 1) rax)
      (mov rax rdi)
      (or rax ,type-pair)
      (add rdi 16))))
)

Now suppose a @racket[map-zero?] operation is added to the language which takes
a single argument, which must be a list of numbers. The operation determines,
for each element of the list, whether the element is 0 or not and produces a
list of the results. In other words, @racket[(map-zero? _xs)] should produce
what @racket[(map zero? _xs)] produces in Racket.

Write a @racket[compile-map-zero?] function that compiles an expression
of the form @racket[(map-zero? _e0)]:

@#reader scribble/comment-reader
(racketblock

;; Expr CEnv -> Asm
(define (compile-map-zero? e0 c)
  ...)
)

You may use any of the x86 registers as scratch registers, with the
exception of @racket['rsp] and @racket['rdi] which need to point to
the stack and heap, respectively.

