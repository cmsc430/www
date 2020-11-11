#lang scribble/manual

@(require (for-label racket)
          "../notes/ev.rkt")

@title{Midterm 2}

@bold{Due: Friday, November 13th 11:59PM}

@(define repo "https://classroom.github.com/a/Gv1cRIkD")

Midterm repository:
@centered{@link[repo repo]}

The repository contains a single markdown file @tt{m2.md}, which you can edit
to submit your answers to the following questions.  Your submission must be
pushed by 11:59pm on Friday (unless you have already arranged otherwise).

During the 72 hours of the exam period, you may only ask private
questions on Discord if you need assistance for the course staff.  You
may not communicate or collaborate with any one else about the content
of this exam.

@section[#:tag-prefix "m2"]{Short answer}

@bold{Question 1}

[12 points]

Using only @racket[cons], @racket['()], symbols, and literal numbers,
strings, booleans, or characters, write expressions that are
equivalent to each of the following:

@itemlist[

@item{@verbatim{'(y y z (5) 2)}}

@item{@verbatim{'((1 2) . 2)}}

@item{@verbatim{'(a . (b . (c)))}}

@item{@verbatim{'(a . (b . c))}}

@item{@verbatim{`(,(add1 1) . 2)}}

@item{@verbatim|{`(,@'(1 2) ,@'(x) ,3)}|}
]

For example, @racket['(1 2 3)] is equivalent to @racket[(cons 1 (cons 2 (cons 3 '())))].

@bold{Question 2}

[10 points]

Is it possible for the following program to run out of memory? Justify your
answer.

@#reader scribble/comment-reader
(racketblock
(define (fact x)
  (if (<= x 1)
      1
      (* x (fact (sub1 x)))))
)

How about this one?  Again, justify your answer.

@#reader scribble/comment-reader
(racketblock
(define (fact x)
  (define (fact-prime acc i)
    (if (<= i 1)
        acc
        (fact-prime (* acc i) (sub1 i))))
  (fact-prime 1 x))
)

Hint: consider Jig.

@bold{Question 3}

[8 points]

For each of the following expressions, which subexpressions are in tail
position? Assume that the top-level expression is in tail position.

@itemlist[

@item{@verbatim{(if (if a0 a1 a2) e1 e2)}}

@item{@verbatim{(match e0 [p1 e1] [p2 e2])}}

@item{@verbatim{(if e0 (if a0 a1 a2) e2)}}

@item{@verbatim{(add1 e0)}}

@item{@verbatim{(cons e0 e1)}}
          
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
this code will be added to the Knock compiler and may use any functions
given to you.)

@#reader scribble/comment-reader
(racketblock
;; LExpr LExpr CEnv -> Asm
;; Compiler for (set-box! e0 e1)
(define (compile-set-box! e0 e1 c) ...)
)

@section[#:tag-prefix "m2"]{Program Transformation: Inlining}

@bold{Question 5}

[20 points]


In our @secref["Iniquity"] compiler we introduced function definitions to help
us organize our code. Function definitions came along with function @emph{calls},
and unfortunately function calls come with a performance hit.

One solution is @emph{inlining}: Taking the body of a funtion definition and
replacing a call to that function with an instance of its body. This allows the
programmer to have all the benefits of the function abstraction, without the
overhead.

For example, consider the following program:

@#reader scribble/comment-reader
(racketblock
(begin
  (define (f x) (+ x 5))
  (+ 10 (f 42)))
)

If you inlined @tt{f}, the result would be the following program:

@#reader scribble/comment-reader
(racketblock
(begin
  (define (f x) (+ x 5))
  (+ 10 (+ 42 5)))
)


Your task is to write and document a function named @tt{inline} which takes a
function @tt{f} and a program @tt{p}, and inlines the function @tt{f} @emph{at
all uses of @tt{f} in the program} (this includes other function definitions!).


@#reader scribble/comment-reader
(racketblock
(define (inline f p)
  ;TODO
)
)


You can assume that you have a function @tt{fvs} that given an expression,
returns a list of all the free variables in that expression. This is be
necessary to avoid creating incorrect programs. If dealing with variable
clashes and creating new names seems too complicated, you can assume the
existence of a function @tt{barendregtify} which given a program makes sure
that all variables are unique, running it on the following:


@#reader scribble/comment-reader
(racketblock
(begin
  (define (f x) (+ x 5))
  (define (g x) (+ x 10))
  (f (g 10)))
)

Would produce something like:


@#reader scribble/comment-reader
(racketblock
(begin
  (define (f var1) (+ var1 5))
  (define (g var2) (+ var2 10))
  (f (g 10)))
)

Why should you care about @tt{fvs} or @tt{barendregtify}? Well, consider the
following inlining:

@#reader scribble/comment-reader
(racketblock
(begin
  (define (f x) (let ((y 5)) x))
  (let ((y 42)) (f y)))
)

If you inline @tt{f} without care, you would get the following:

@#reader scribble/comment-reader
(racketblock
(begin
  (define (f x) (let ((y 5)) x))
  (let ((y 42)) (let ((y 5)) y)))
)

Now you've changed the program, it would result in 5 instead of 42!
You can use @tt{fvs} to figure out which variables might be captured,
and deal with that, or you can use @tt{barendregtify} to make sure
that all variables are unique. Notice how if we had used @tt{barendregtify}
we could have avoided the problem:

@#reader scribble/comment-reader
(racketblock
(begin
  (define (f var1) (let ((var2 5)) var1))
  (let ((var3 42)) (f var3)))
)

Then inlining @tt{f} we get:

@#reader scribble/comment-reader
(racketblock
(begin
  (define (f var1) (let ((var2 5)) var1))
  (let ((var3 42)) (let ((var2 5)) var3)))
)

You may have to run/call @tt{barendregtify} more than once to ensure
correctness, when inlining functions.


In addition to your code, make sure you answer the following question as part
of your submission:

@itemlist[

@item{Recursive functions refer to themselves... What should you do? (You do
not have to worry about mutual recursion between sets of functions, for this
exam we will assume either a function refers to itself directly, or it is not
recusive at all).}

]

Other things you should consider:

@itemlist[

@item{If @tt{f} does not exist in the program, the program should be unchanged}

@item{You have to be careful with variable shadowing/capture.}

]

Part A:

5 points from the total of 20 will be given if you can produce the correct
inlining of @tt{f} for the following program (you are free to do this by hand):

@#reader scribble/comment-reader
(racketblock
(begin
  (define (f x) (let ((y (+ x 10))) (let ((z 42)) (+ x (+ y z)))))
  (define (g x) (f x))
  (let ((y 1024)) (g (f y))))
)

Part B:

15 points for completing the implementation below:

@#reader scribble/comment-reader
(racketblock
(define (inline f p)
  (match p
    [(prog ds e)
          ;TODO
     ]))
)


@bold{Question 5 Extra Credit}

[10 points]

Provide two graphs in dot format (explained in our lecture on Graphviz/Dot)
that shows the difference between interpreters and compilers. You can lay this
out however you'd like, but the following items should definitely be present in
one, or both, of the graphs (though not necessarily connected in the diagram!).
These are in alphabetical order, so don't assume that their order reveals
anything about how the diagram should be layed out.

@itemlist[

@item{Source Language}
@item{Target Language}
@item{AST}
@item{Code-Gen}
@item{CPU Execution}
@item{Interp}
@item{Parser}
@item{RTS}
@item{Source Language}
@item{Target Language}
@item{Value/Result}

]

In order to get the points for this, we will take your description in dot and
run @tt{dot} on it.  If it does not produce an output, you will get not points.

If it produces an output that is a good-faith effort (i.e. there is a clear
attempt and showing the difference between compilers and interpreters, you will
get 5 points. If it accurately shows the difference you will get 10 points.
