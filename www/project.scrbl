#lang scribble/manual
@(require "defns.rkt")
@(require "notes/ev.rkt")

@title[#:style '(unnumbered)]{Project}

The final assesment for this course consists of an individually
completed project.

Final deliverables are due by the end of the time schedule for the
class's final exam as set by the registrar.

Submissions should be made on Gradescope.

There are several projects to choose from, described below.

In addition the source code for your project, you must write a 2-page
document which gives a summary of your work and describes how your
project is implemented.

@section{a86 optimizer}

Our compiler is designed to be simple and easy to maintain.  That
comes at the cost of emitting code that often does needless work.
Write an a86 optimizer, i.e., a program that takes in a list of a86
instructions and produces an alternative list of instructions that
have the same behavior, but will execute more efficiently.

This is a fairly open-ended project, which means you can take a simple
approach, or you can do a deep-dive on assembly code optimization and
try to do something very sophisticated.

For a maximum of 95% of the possible points, your optimizer should
work on any a86 instructions produced by the
@seclink["Iniquity"]{Iniquity} compiler.  For 100%, your optimizer
should work on any a86 instructions produced by the
@seclink["Loot"]{Loot} compiler.

The most important aspect of the optimizer is it must preserve the
meaning of the original source program.  If running a program with or
without optimization can produce different results, you will lose
significant points.

The second important aspect of the optimizer is that it produces more
efficient code (but this should never come at the expense of
correctness---otherwise it's trivial to optimize every program!).  You
should design some experiments demonstrating the impact of your
optimizations and measure the performance improvement of your optimizer.

Here are some ideas for what you can optimize:

@itemlist[

@item{Avoid stack references where possible.

For example, you might push something and immediately reference it:
@racket[(seq (Push _r1) (Mov _r2 (Offset rsp 0)))], which is
equivalent to @racket[(seq (Push _r1) (Mov _r2 _r1))].  The
register-to-register move will be faster than accessing the memory on
the stack.}

@item{Avoid stack pushes where possible.

In the previous example, it may be tempting to delete the
@racket[Push], but that is only valid if that stack element is not
referenced later before being popped.  And even if the element is not
referenced, we have to be careful about how the element is popped.

But if you know where the pop occurs and there's no intervening
references in to the stack or other stack changes, then you can
improve the code further, e.g. @racket[(seq (Push _r1) (Mov _r2
(Offset rsp 0)) (Add rsp 8))] can become @racket[(seq (Mov _r2 _r1))].
}

@item{Statically compute.

Sometimes the compiler emits code for computing something at run-time
which can instead be computed at compile time.  For example, the
compiler might emit @racket[(seq (Mov _r 42) (Add _r 12))], but this
can be simplified to @racket[(seq (Mov _r 54))].}

]

There are many, many other kinds of optimizations you might consider.
To get a sense of the opportunities for optimization, try compiling
small examples and looking at the assembly code produces.  Try
hand-optimizing the code, then try to abstract what you did by hand
and do it programmatically.

@section{Source optimizer}

Another complimentary approach to making programs compute more
efficiently is to optimize them at the level of source code.  Write a
source code optimizer, i.e. a program that takes in a program AST and
produces an alternative AST that has the same behavior, but will
execute more efficiently.

This is another fairly open-ended project, which means you can take a
simple approach, or you can do a deep-dive on source code optimization
and try to do something very sophisticated.

For a maximum of 95% of the possible points, your optimizer should
work for the @seclink["Iniquity"]{Iniquity} language.  For 100%, your
optimizer should work for the @seclink["Loot"]{Loot} language (or later).

The most important aspect of the optimizer is it must preserve the
meaning of the original source program. If running a program with or
without optimization can produce different results, you will lose
significant points.

The second important aspect of the optimizer is that it produces more
efficient code (but this should never come at the expense of
correctness—otherwise it’s trivial to optimize every program!). You
should design some experiments demonstrating the impact of your
optimizations and measure the performance improvement of your
optimizer.

Here are some ideas for where you can optimize:

@itemlist[

@item{Avoid variable bindings where possible.

Sometimes a program may bind a variable to a value, but then use the
variable only once, e.g. @racket[(let ((x (add1 7))) (add1 x))].  We
can instead replace the variable occurrence with it's definition to
get: @racket[(add1 (add1 7))].  Note that can must be taken to
@emph{not} do this optimization if it changes the order in which
effects may happen.  For example, consider

@racketblock[
(let ((x (read-byte)))
  (begin (read-byte)
         (add1 x)))
]

This is not the same as:

@racketblock[
(begin (read-byte)
       (add1 (read-byte)))
]

because the latter adds one to the second byte of the input stream rather than the first.}

@item{Statically compute.

Sometimes parts of a program can be computed at compile-time rather
than run-time.  For example, @racket[(add1 41)] can be replaced with
@racket[42].  Likewise, expressions like @racket[(if #f _e1 _e2)] can
be replaced by @racket[_e2].}

@item{Inline function calls.

Suppose you have:

@racketblock[
(define (f x) (add1 x))
(if (zero? (f 5)) _e1 _e2)
]

Since the expression @racket[(f 5)] is calling a known function, you
should be able to transform this call into @racket[(let ((x 5)) (add1
x))].  Using the previously described optimization, you can further
optimize this to @racket[(add1 5)], which in turn can be simiplified
to @racket[6].  You can keep going and notice that @racket[(zero? 6)]
is just @racket[#f], so the whole program can be simplified to:

@racketblock[
(define (f x) (add1 x))
_e2
]
}

]

Note that the last example can get considerably more complicated in a
language with first-class functions since it may not be possible to
know statically which function is being called.

There are many other optimziations you might consider.  Think about
the kinds of expressions you might write and how they can be
simplified, then figure out how to do it programmatically.

@section{Multiple return values}

Racket, Scheme, and even x86 support returning more than one value
from a function call.  Implement Racket's @racket[let-values] and
@racket[values] forms to add multiple return values.

You may choose to implement this feature for any language that is 
@seclink["Iniquity"]{Iniquity} or later for a maximum 95% of the
possible points.  For 100% you'll need to implement the feature for
Loot or later.

Here are the key features that need to be added:

@itemlist[

@item{@racket[(values _e1 ... _en)] will evaluate @racket[_e1] through
@racket[_en] and then ``return'' all of their values.}

@item{@racket[(let-values ([(_x1 ... _xn) _e]) _e0)] will evaluate
@racket[_e], which is expected to be an expression that produces
@racket[_n] values, which are bound to @racket[_x1] through
@racket[_xn] in the body expression @racket[_e0].}

]


Here are some examples to help illustrate:

@ex[

(let-values ([(x y) (values 1 2)]) (+ x y))

(let-values ([(x) (values 1)]) (add1 x))

(let-values ([() (values)]) 7)

(define (f x)
  (values x (+ x 1) (+ x 2)))
  
(let-values ([(x y z) (f 5)])
  (cons x (cons y (cons z '()))))

(add1 (values 5))

(let ((x (values 5)))
  (add1 x))

]

Any time an expression produces a number of values that doesn't match
what the surrounding context expects, an error should be signalled.

@ex[

(eval:error (add1 (values 1 2)))

(eval:error (let-values ([(x y) 2]) x))

]

The top-level expression may produce any number of values and the
run-time system should print each of them out, followed by a newline:

@ex[
(values 1 2 3)
]

Note there is some symmetry here between function arity checking where
we make sure the number of arguments matches the number of parameters
of the function being called and the ``result arity'' checking that is
required to implement this feature.  This suggests a similiar approach
to implementing this feature, namely designating a register to
communicate the arity of the result, which should be checked by the
surrounding context.

You will also need to design an alternative mechanism for
communicating return values.  Using a single register (@racket['rax])
works when every expression produces a single result, but now
expressions may produce an arbitrary number of results and using
registers will no longer suffice.  (Although you may want to continue
to use @racket['rax] for the common case of a single result.)  The
solution for this problem with function parameters was to use the
stack and a similar approach can work for results too.

@section{Exceptions and Exception Handling}

Exceptions and exception handling mechanisms are widely used in modern
programming languages.  Implement Racket's @racket[raise] and
@racket[with-handlers] forms to add exception handling.

You may choose to implement this feature for any language that is
@seclink["Iniquity"]{Iniquity} or later for a maximum 95% of the
possible points.  For 100% you'll need to implement the feature for
Loot or later.

@subsection{Requirements}

Here are the key features that need to be added:

@itemlist[

@item{@racket[(raise _e)] will evaluate @racket[_e] and then ``raise''
the value, side-stepping the usual flow of control and instead jump
to the most recently installed exception handler.}

@item{@racket[(with-handlers ([_p1 _f1] ...) _e)] will install a new
exception handler during the evaluation of @racket[_e].  If
@racket[_e] raises an exception that is not caught, the predicates
should be applied to the raised value until finding the first
@racket[_pi] that returns true, at which point the corresponding
function @racket[_fi] is called with the raised value and the result
of that application is the result of the entire @racket[with-handlers]
expression.  If @racket[_e] does not raise an error, its value is the
value of the @racket[with-handler] expression.}

]

Here are some examples to help illustrate:

@ex[

(with-handlers ([string? (λ (s) (cons "got" s))])
  (raise "a string!"))

(with-handlers ([string? (λ (s) (cons "got" s))]
                [number? (λ (n) (+ n n))])
  (raise 10))

(with-handlers ([string? (λ (s) (cons "got" s))]
                [number? (λ (n) (+ n n))])
  (+ (raise 10) 30))

(let ((f (λ (x) (raise 10))))
  (with-handlers ([string? (λ (s) (cons "got" s))]
                  [number? (λ (n) (+ n n))])
    (+ (f 10) 30)))

(with-handlers ([string? (λ (s) (cons "got" s))]
                [number? (λ (n) (+ n n))])
  'nothing-bad-happens)

(with-handlers ([symbol? (λ (s) (cons 'reraised s))])
  (with-handlers ([string? (λ (s) (cons "got" s))]
                  [number? (λ (n) (+ n n))])
    (raise 'not-handled-by-inner-handler)))

]

Notice that when a value is raised, the enclosing context is discard.
In the third example, the surrounding @racket[(+ [] 30)] part is
ignored and instead the raised value @racket[10] is given the
exception handler predicates, selecting the appropriate handler.

Thinking about the implementation, what this means is that a portion
of the stack needs to be discarded, namely the area between the
current top of the stack and the stack that was in place when the
@racket[with-handlers] expression was evaluated.

This suggestions that a @racket[with-handlers] expression should stash
away the current value of @racket['rsp].  When a @racket[raise]
happens, it grabs the stashed away value and installs it as the
current value of @racket['rsp], effectively rolling back the stack to
its state at the point the exception handler was installed.  It should
then jump to code that will carry out the applying of the predicates
and right-hand-side functions.

Since @racket[with-handler]s can be nested, you will need to maintain
an arbitrarily large collection of exception handlers, each of which
has a pointer into the stack and a label for the code to handle the
exception.  This collection should operate like a stack: each
@racket[with-handlers] expression adds a new handler to the handler
stack.  If the body expression returns normally, the top-most handler
should be removed.  When a raise happens, the top-most handler is
popped and used.

@;{
@subsection{Additional requirements}

To receive full credit, you will to add the above features to Perp and
do the following.

After you have a working implementation of @racket[raise] and
@racket[with-handlers], add a structure definition to your standard
library: @racket[(struct exn:fail (msg cm))].  Rework the compiler so
that all run-time errors raise an instance of @racket[struct:fail].
This enables user-programs to handle run-time errors like this:

@ex[

(with-handlers ([exn:fail? (λ (e) 'OK)])
  (add1 #f))

]

(The @racket[cm] field can be ignored; you can always populate it with
@racket[#f] if you'd like.  It's there just for consistency with
Racket's @racket[exn:fail].)
}



@;{
For your project you should turn in your extension code, a directory
of examples that showcase your extension and the differences in
behavior compared to the original language, and a @bold{short}
paragraph describing anything you found interesting. Submission will
be handled by gradescope, with more details to follow.

The suggested deadline is the last date of classes - May 11th - to
avoid overlap with finals week. That said, you're free to take a few
extra days and submit after that deadline until the nominal date
of the final exam (which would have been on Tuesday, May 18th if it
was happening). That is a strict deadline, not imposed by me, so make
sure you have turned in your projects by then.

As a first step, you have to pick a project idea as your "assignment"
for this week. Just write a @bold{short} paragraph with your project
of choice: what you hope to accomplish and a high-level description of
your approach. This assignment should be live on Gradescope.

Here are some project ideas that have been discussed throughout the
semester:

@itemlist[

  @item{Error handling. Currently, our languages return a
  not-very-informative @tt{'err} symbol when things go wrong. Real
  languages offer a lot more information: the reason that something
  went wrong, context, expressions involved, file name, line numbers,
  etc. This project would aim to improve the error reporting for Loot.
  Improving that behavior for an interpreter is pretty straightforward
  and should be an easy first step for this project until you're happy
  with the output error messages.  Porting that better error behavior
  on the compiler is a bit more involved - there are multiple possible
  approaches to this, but hacking on the runtime system is always an
  option!
  }

  @item{Typing Loot. We have discussed typing for @tt{Hustle} and its
  implications in the compiler (deleting a whole lot of assertions), as
  well as typing for a simple λ calculus. This project would aim
  to combine the two threads, implementing a type system on top of Loot.
  There are interesting design decisions here, so feel free to reach out
  to talk about them!}

  @item{Loot Optimizations. Sky's the limit here. You can try
  high-level optimizations (e.g. inlining, λ lifting, dead-code
  elimination, partial evaluation, etc.) or low-level ones (register
  allocation, register-based calling conventions etc.). Optimizations
  can be tricky to get right, so make sure you reuse all the unit
  tests we have provided throughout the semester and expand upon them!}

  @item{Whatever feature you want to add! Get in touch to discuss whether
  its scope is appropriate for a final project.}
]


@;{


@(define repo "https://classroom.github.com/a/t5KO9b5-")

The goal of this project is to put together everything you've
learned over the semester to complete a full-featured compiler.

Project repository:
@centered{@link[repo repo]}

@link["code/project.pdf"]{Slides} from lecture on the project.

You are given a working compiler for an extension of the language we
have been developing all semester.

Your overall object is to improve the @emph{run-time} performance of
code generated by your compiler while maintaining correctness.

There will be two releases of benchmark programs:

@itemlist[
@item{Tuesday 12/3}
@item{Tuesday 12/10}
]

The final due date for your project is 10:30 AM on Saturday 12/14.

You will have an allowance of 10 minutes to @emph{compile} all benchmark
programs.  Exceeding the allowance result in a penalty, but there is
no reward for improving @emph{compile-time} performance so long as you
come in under the 10 minute mark.

You will have an allowance of 10 minutes to @emph{run} all benchmark
programs.  For full-credit, you must improve the overall run-time
performance by 20\%.  Run-time will compute as the average of three
runs, done on the GRACE cluster.

Full credit solutions will be entered in a compiler tournament to
determine the most performant (and correct) compiler.  Tournament
results do not count toward your grade and will involve compiling
programs not included in the benchmark suite.

Benchmark programs will be batch I/O programs: read some input,
compute something, produce a result and/or write some output.

I/O primitives include @racket[read-char], @racket[write-char]
(limited to the standard input and output ports).

The compiler supports a standard library, with source level
definitions provided to you.  See the @racket[stdlib] function 
in the compiler.

There will be a garbage collector provided by the second round of
benchmarks which you will need to incorporate in to your compiler.

@section[#:tag-prefix "fp-" #:style 'unnumbered]{Measuring run-times}

Let's look at an example of how to measure the run-time performance of
the code your compiler generates.

First, let's start with fairly computationally intensive program.
Here is a @link["code/fp/sieve.rkt"]{program} that computes the
@emph{n}th prime number using the ancient
@link["https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes"]{Sieve of
Eratosthenes} method.

Save it to the directory where your compiler lives and run @tt{make
sieve.run}.  This will run the compiler to generate the @tt{sieve.run}
executable.  This program expects to read a number from the standard
input port.

Run:

@centered{@tt{echo -n 100 | ./sieve.run}}

to compute the 100th prime number.

To measure the time it takes, add the time command:

@centered{@tt{echo -n 100 | time ./sieve.run}}

This will run the program and show the result @emph{and} timing
information.  We will be concerned with improving the real time it
takes to run the program.


@section[#:tag-prefix "fp-" #:style 'unnumbered]{Testing}

@bold{There is separate a repository for tests.} When you push your
code, Travis will automatically run your code against the tests.  If
you would like to run the tests locally, clone the following
repository into the directory that contains your compiler and run
@tt{raco test .} to test everything:

@centered{@tt{https://github.com/cmsc430/fp-test.git}}

This repository will evolve as the week goes on, but any time there's
a significant update it will be announced on Piazza.

@section[#:tag-prefix "fp-" #:style 'unnumbered]{Submitting}

Pushing your local repository to github ``submits'' your work.  We
will grade the latest submission that occurs before the deadline.
}

}

@section{Design your own}

You may also design your own project, however, you will need to submit
a one-page write-up that documents what you plan to do and how you
will evaluate whether it is successful.  You must submit this document
and have it approved by the instructor by Dec 7.
