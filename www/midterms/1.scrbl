#lang scribble/manual

@(require (for-label racket))

@title{Midterm 1}

@bold{Due: Tuesday, March 9th 11:59PM}

@(define repo "https://github.com/cmsc430/Midterm1-prog")

Midterm repository:
@centered{@link[repo repo]}

The exam consists of two parts: a written portion and a programmatic
portion.  Both will be handled through gradescope. You will see two
gradescope assignments marked accordingly.

During the 72 hours of the exam period, you may only ask private questions to
the staff (via email, discord, etc.) if you need clarification.
You may not communicate or collaborate with any one else about the
content of this exam.

Questions that are deemed applicable to the entire class will be shared, along
with their responses, with the rest of the class.

The repository contains three things.
@itemlist[
@item{A plaintext file @tt{m1.txt}, which you can edit to submit your answers to the written portion.}
@item{A folder @tt{VariadicXor} which contains the base code to build upon for Question 4.}
@item{A folder @tt{Optimizer} which contains the base code to build upon for Question 5.}
]

Your submission must be submitted by 11:59 EDT on Tuesday, March
9th. For the programmatic fragment, you should submit a zip file
containing @emph{only} two files: the @tt{compiler.rkt} for the
@tt{VariadicXor}, and @tt{optimize.rkt} for the @tt{Optimizer}.

@section{Short answer}

@bold{Question 1 - Written}

[10 points]

In the following questions, assume that the value in @tt{rax} is 17
and the value in @tt{rsp} is 2048.

What is the value of @tt{rax} and @tt{rsp} if we execute each of the
following instructions or instruction sequences in that state? 

@itemlist[

@item{
@verbatim{
(Mov 'rax 42)
}
}

@item{
@verbatim{
(Mov 'rax 'rsp)
}
}

@item{
@verbatim{
(Mov 'rax (Offset 'rsp 0))
}
}

@item{
@verbatim{
(Push 'rax)
}
}

@item{
@verbatim{
(Mov 'rax 42)
(Push 'rax)
}
}
]



@bold{Question 2 - written}

[10 points]

Assume that the register @tt{rbx} currently holds the value 1024.
Write two sequences of instructions (in a86):

@itemlist[
@item{
One that stores the value of @tt{rax} in location 1032 (= 1024 + 8).
}
@item{
One that loads the value stored at location 1024 and stores it at @tt{rax}.
}
]

@section{Representation}

@bold{Question 3}

[20 points]

When studying the @secref{Dupe} language, we used the least
significant bit of a 64-bit integer to indicate whether the value
being represented was an integer (tagged with @code[#:lang
"racket"]{#b0}) or a boolean (tagged with @code[#:lang
"racket"]{#b1}).

An alternative approach to disambiguate types at runtime would be to
represent false (@racket[#f]) as 0 (@racket[#b0]), true (@racket[#t])
as 1 (@racket[#b1]), and any integer @tt{n} as @tt{n+2}, if @tt{n > 0}
and as @tt{n} if @tt{n < 0}.

Briefly answer the following questions: 

@itemlist[

@item{What is the range of integers that we can represent with this
approach? How does it compare to the one in Dupe?}

@item{Describe, at a high-level in English, how you could implement
@racket[add1] using this design. How does it compare to the one
in Dupe?}

@item{How would you extend this approach to handle characters? How
would you compare that to the way of generalizing Dupe to Dodger?}

]

@section{Interpreting the Xor Variadic Primitive - programmatic}

@bold{Question 4}

[25 points]

In the repo (@link[repo repo]), you will find a directory named
"VariadicXor".  That contains the @tt{Fraud} language from the
lectures, extended with a single additional primitive that takes an
arbitrary number of arguments: @racket[bitwise-xor].

In racket, @racket[bitwise-xor] performs the @tt{xor} operation cumulatively
at its arguments, at the bitlevel. For example:

@#reader scribble/comment-reader
(racketblock
; 0 is the neutral element for xor
(bitwise-xor) = 0

; xor with a single numeric argument yields the argument itself
(bitwise-xor 0) = 0
(bitwise-xor 1) = 1

; xor with a single non-numeric argument yields an error
(bitwise-xor #t) ; contract violation

; xor with two arguments is their bitwise xor
(bitwise-xor 1 2) = 3 ; Indeed: 1 = #b01, 2 = #b10 
(bitwise-xor 1 3) = 2 ; Indeed: 1 = #b01, 3 = #b11, 2 = #b10

; xor with multiple arguments just performs the xor in sequence:
(bitwise-xor 1 2 3 4) = (bitwise-xor 1 (bitwise-xor 2 3 4)) = 4
)

Its AST representation and its interpretation are given below - it
basically just applies the racket @racket[bitwise-xor] function
to the interpretation of all of its arguments (without doing
any error handling, to keep things simple).

@#reader scribble/comment-reader
(racketblock
;; AST
...
;; Added compared to Fraud:
(struct BitwiseXor (es)  #:prefab)

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    ...
    ;; Added compared to Fraud:
    [(BitwiseXor es) (apply bitwise-xor (map (lambda (e) (interp-env e r)) es))]
    ...
  )    
)
)

In the directory, you will find the @tt{ast.rkt}, @tt{interp.rkt}, and @tt{parse.rkt}
already implemented. Your task is to extend the compiler in @tt{compiler.rkt} to
implement the same behavior.

Don't worry about what happens if any of the arguments is not an integer or yields an
@racket['err] - the requirement is that the compiler evaluates the bitwise xor of its
arguments, when these arguments evaluate to integers.

@section{Writing a transformation over the AST - programmatic}

@bold{Question 5}

[35 points]

In the @link[repo repo], you will find a directory named "Optimizer".
That contains a simplification of the @tt{Fraud} language from the
lectures, that does not contain characters, conversions between
characters and integers, @racket[begin] or @racket[write-byte].
What's left is a simplified language with integers and booleans,
arithmetic operations and tests, conditionals, let-bindings and
@racket[read-byte]. We provide the AST, interperter and parser.

Your task is to write an optimizer for this language
(@tt{optimize.rkt}) that transforms the AST of a given expression
evaluating as much as possible.  What does that mean? The majority of
programs in this language contain constants (integer or
boolean). These constants are known statically, which means that we
can transform our programs to evaluate entire sub-expressions and
replace them with the result. Note however, that some values are
unknown until runtime (those that come from @racket[read-byte]). Your
optimizer should leave the behavior of programs containing
@racket[read-byte] unchanged, while still evaluating all fully-known
sub-expressions.

Let's look at some examples:

@subsection{Example 1 - Arithmetic Expressions}

@#reader scribble/comment-reader
(racketblock
(+ 1 (- 43 2))
)

Would be transformed to a single integer:

@#reader scribble/comment-reader
(racketblock
42
)

@subsection{Example 2 - Arithmetic with read-byte}

@#reader scribble/comment-reader
(racketblock
(+ (read-byte) (+ 3 4))
)

Becomes:

@#reader scribble/comment-reader
(racketblock
(+ (read-byte) 7)
)

The second sub-expression @racket[(+ 3 4)] can be replaced by its
result (@racket[7]) but @racket[read-byte] must remain unchanged.

@subsection{Example 3 - Conditionals}

@#reader scribble/comment-reader
(racketblock
(if (zero? 17) (read-byte) 4)
)

Becomes a single integer again:

@#reader scribble/comment-reader
(racketblock
4
)

Because we know that @racket[(zero? 17)] can evaluate to @racket[#f],
we can simplify the entire @racket[if] to its else branch.

@subsection{Example 4 - Conditionals with read-byte}

@#reader scribble/comment-reader
(racketblock
(if (zero? (read-byte) 1 (zero? 0)))
)

Becomes:

@#reader scribble/comment-reader
(racketblock
(if (zero? (read-byte) 1 #t))
)

Since we don't know the value of @racket[read-byte] statically,
we can't simplify the conditional. However, we can simplify
@racket[(zero? 0)] to @racket[#t].

@subsection{Example 5 - Let Bindings}

@#reader scribble/comment-reader
(racketblock
(let ((x 7)) (+ x 8))
)

Becomes a single integer yet again:

@#reader scribble/comment-reader
(racketblock
15
)

The value that is bound to @racket[x] is known statically
and therefore we can use it in the body of the let.

@subsection{Example 6 - Let Bindings with read-byte}

@#reader scribble/comment-reader
(racketblock
(let ((x (+ (add1 2) (read-byte)))) (+ x (- 4 3)))
)

Becomes:

@#reader scribble/comment-reader
(racketblock
(let ((x (+ 3 (read-byte)))) (+ x 1))
)

This time the value bound to x is not statically known, so we can't
simplify the let-binding. However, we can simplify some
sub-expressions @racket[(add1 2)] and @racket[(- 4 3)] to @racket[3]
and @racket[1] respectively.

@subsection{Example 7 - Don't forget about nested expressions!}

@#reader scribble/comment-reader
(racketblock
(+ (+ 1 2) (+ 3 4))
)

Becomes a single integer once more:

@#reader scribble/comment-reader
(racketblock
10
)

The two sub-expressions evaluate to @racket[3] and @racket[7],
which then allows us to evaluate the outer addition to @racket[10].

@subsection{Example 8 - Nested Expressions with read-byte}

@#reader scribble/comment-reader
(racketblock
(+ (+ 1 (read-byte)) (+ (read-byte) 4))
)

This expression remains unchanged.

No sub-expression can be fully evaluated and replaced with its result
due to the presence of @racket[read-byte]. In principle, one could use
the commutativity of addition to try and add @racket[1] and
@racket[4].  @emph{Do not attempt to do that!} That turns a slightly
interesting but mostly straightforward midterm question to something
that would probably be within scope for a final project! (Also it
will throw off the autograder)

@subsection{Submission and Grading}

For this question, you must replace the placeholder implementation in
@tt{optimize.rkt} (which just returns the input expression), with your
own optimizer. This question will be graded on two axes: correctness
(15 points) and efficiency (20 points). Note that the placeholder
solution gets all 15 correctness points - it is correct by
definition! If you want to aim for the full score, you'll have to
implement various bits of functionality - but be careful: you risk
losing correctness points otherwise!
