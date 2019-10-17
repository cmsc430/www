#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  "hustle/semantics.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "loot" f))))))
	   '("interp.rkt" "compile.rkt" #;"asm/interp.rkt" #;"asm/printer.rkt"))

@title[#:tag "Loot"]{Loot: lambda the ultimate}

@table-of-contents[]

@section[#:tag-prefix "loot"]{Functions in their most general form}

We've been building up the pieces of functions, first with
second-class functions, then with tail-calls, then with first-class
function pointers.  You've seen variable arity functions and the
@racket[apply] operator in @seclink["Assignment 6"]{Assignment
6}.

Now we're ready to deal with functions in their most general form:
@racket[λ]-expressions.


We add @racket[λ]-expressions to the syntax and remove the
@racket[(fun ,Variable)] and @racket[(call ,Expr ,@(Listof Expr))]
forms.  We no longer need a separate syntactic form for referencing
the name of a function, we can just use variable binding.  Likewise,
we use the same syntax as Racket for function application:

@verbatim|{
;; type Expr =
;; | ....
;; | `(λ ,Formals ,Expr)
;; | `(,Expr ,@(Listof Expr))
}|

For the moment, @tt{Formals} can be defined as a list of variables:

@verbatim|{
;; type Formals = (Listof Variable)
}|

But it's possible to extend the @racket[λ]-notation to include the
ability to define variable-arity functions, along the lines of
@secref["Assignment 6"].

@section[#:tag-prefix "loot"]{Long Live Lambda!}

Let's start by developing the interpreter for Loot, where the relevant
forms are @racket[λ]s and applications:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv -> Answer
(define (interp-env e r)
    ;;...
    [`(λ (,xs ...) ,e)  '...]
    [`(,e . ,es)        '...])
)

These two parts of the interpreter must fit together: @racket[λ] is
the constructor for functions and application is deconstructor.  An
application will evaluate all its subexpressions and the value
produced by @racket[_e] ought to be the kind of value constructed by
@racket[λ].  That value needs to include all the necessary information
to, if given the values of the arguments @racket[es], evaluate the
body of the function in an environment associating the parameter names
with the arguments' values.

So how should functions be @emph{represented}?  Here is a simple idea
following the pattern we've used frequently in the interpreter:

@itemlist[
@item{Q: How can we represent strings?}
@item{A: With strings!}

@item{Q: How can we represent booleans?}
@item{A: With booleans!}

@item{Q: How can we represent numbers?}
@item{A: With numbers!}

@item{Q: How can we represent pairs?}
@item{A: With pairs!}

@item{Q: etc.}
@item{A: etc.}
]

So now:
@itemlist[
@item{Q: How can we represent functions?}
@item{A: With functions!?}
]

Great, so we will use function to represent functions.  We can
implement function application with function application.  Let's fill
in what we know so far:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv -> Answer
(define (interp-env e r)
    ;;...
    [`(λ (,xs ...) ,e)
     (λ ??? '...)]
    [`(,e . ,es)
     (let ((f (interp-eval e r))
           (vs (interp-eval* es r)))
       (apply f vs))])
)


It's not totally clear what parameters the representation of a
function should have or what we should in the body of that function.
However, the code in the interpretation of an application sheds light
on both.  First, it's clear a function should potentially take any
number of arguments:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv -> Answer
(define (interp-env e r)
    ;;...
    [`(λ (,xs ...) ,e)
     (λ vs '...)]
    [`(,e . ,es)
     (let ((f (interp-eval e r))
           (vs (interp-eval* es r)))
       (apply f vs))])
)


Second, what should happen when a function is applied?  It should
produce the answer produced by the body of the @racket[λ] expression
in an environment that associates @racket[xs] with @racket[vs].
Translating that to code, we get:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv -> Answer
(define (interp-env e r)
    ;;...
    [`(λ (,xs ...) ,e)
     (λ vs (interp-env e (zip xs vs)))]
    [`(,e . ,es)
     (let ((f (interp-eval e r))
           (vs (interp-eval* es r)))
       (apply f vs))])
)

And now we have simultaneously arrived at our representation of function values:
@#reader scribble/comment-reader
(racketblock
;; type Value =
;; | ....
;; | (Value ... -> Answer)
)

and completed the implementation of the interpreter.

There are, however, problems.

For one, this approach does not model how @racket[λ]-expressions are
able to capture the environment in which they are evaluated.  Consider:

@racketblock[
(let ((y 8))
  (λ (x) (+ x y)))
]

This evaluates to a function that, when applied, should add 8 to its
argument.  It does so by evaluating the body of the @racket[λ], but in
an environment that both associates @racket[x] with the value of the
argument, but also associates @racket[y] with @racket[8].  That
association comes from the environment in place when the
@racket[λ]-expression is evaluated.  The interpreter as written will
consider @racket[y] is unbound!

The solution is easy: in order for (Loot) functions to capture their
(implicit) environment, we should capture the (explicit) environment
in the (Racket) function:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv -> Answer
(define (interp-env e r)
    ;;...
    [`(λ (,xs ...) ,e)
     (λ (vs) (interp-env e (append (zip xs vs) r)))]
    [`(,e . ,es)
     (let ((f (interp-eval e r))
           (vs (interp-eval* es r)))
       (apply f vs))])
)

The last remaining issue is we should do some type and arity-checking:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv -> Answer
(define (interp-env e r)
    ;;...
    [`(λ (,xs ...) ,e)
     (λ (vs)
       (if (= (length xs) (length vs))
           (interp-env e (append (zip xs vs) r))
	   'err))]
    [`(,e . ,es)
     (let ((f (interp-eval e r))
           (vs (interp-eval* es r)))
       (if (procedure? f)
           (apply f vs)
	   'err))])
)


The complete interpreter is:

@codeblock-include["loot/interp.rkt"]


We now have the full power of @racket[λ] expressions in our language.
We can write recursive functions, using only anonymous functions, via
the Y-combinator:

@ex[
(interp
  '(λ (t)
     ((λ (f) (t (λ (z) ((f f) z))))
      (λ (f) (t (λ (z) ((f f) z)))))))
]

For example, computing the triangular function applied to 10:

@ex[
(interp
  '(((λ (t)
       ((λ (f) (t (λ (z) ((f f) z))))
        (λ (f) (t (λ (z) ((f f) z))))))
     (λ (tri)
       (λ (n)
         (if (zero? n)
             1
             (+ n (tri (sub1 n)))))))
    10))
]

One of the niceties of using Racket functions to represent Loot
functions is we can define Racket functions via the interpretation of
Loot functions:

@ex[
(define Y
  (interp
    '(λ (t)
       ((λ (f) (t (λ (z) ((f f) z))))
        (λ (f) (t (λ (z) ((f f) z))))))))

(define tri
  (interp '(λ (tri)
             (λ (n)
               (if (zero? n)
                   1
                   (+ n (tri (sub1 n))))))))
]

And then use them from within Racket:

@ex[
((Y tri) 10)
]

We can also ``import'' Racket functions in to Loot:

@ex[
(interp-env '(expt 2 10)
            `((expt ,expt)))
]


@section[#:tag-prefix "loot"]{Lambda is Dead!}

Now the question you might naturally wonder is: how does implementing
functions in terms of functions help me implement functions in x86,
which after all, doesn't have @racket[λ]?

The answer is that from this point, in which we have an understandable
account of functions, we can iteratively revise the interpreter to
eliminate the use of functions while computing equivalent results.
Doing so will shed light on the lower-level implementation of
functions in the compiler.

Consider again what it is that a @racket[λ]-expression is doing for
you:

@itemlist[

@item{it is packaging up the parameters, body, and environment, so
that}

@item{when applied it can evaluate the body, binding the parameters,
in an extension of the functions environment.}

]

We can achive these things without using a function value by:

@itemlist[
@item{creating a data structure to hold the parameters, body, and environment, and}
@item{rewriting the application of the function to use those values to evaluate
the body, binding the parameters, etc.}
]

So we are changing the representation of functions from:

And now we have simultaneously arrived at our representation of function values:
@#reader scribble/comment-reader
(racketblock
;; type Value =
;; | ....
;; | (Value ... -> Answer)
)

To:

@#reader scribble/comment-reader
(racketblock
;; type Value =
;; | ....
;; | `(closure ,Formals ,Expr ,Env)
)

When a @racket[λ] is evaluated, a closure is created.  When a function
is applied, we deconstruct the closure and execute the code that used
to be in the (Racket) function:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv -> Answer
(define (interp-env e r)
    ;;...
    [`(λ (,xs ...) ,e)
     `(closure ,xs ,e ,r)]
    [`(,e . ,es)
     (let ((f (interp-eval e r))
           (vs (interp-eval* es r)))
       (match f
         [`(closure ,xs ,e ,r)
	  (if (= (length vs) (length xs))
              (interp-env e (append (zip xs vs) r))
	      'err)]
	 [_ 'err]))])
)

We can give it a try:


@(ev `(require (file ,(path->string (build-path notes "loot" "interp-defun.rkt")))))

@ex[
(interp '(λ (x) x))
(interp '((λ (x) (λ (y) x)) 8))
]

Notice in the second example how the closure contains the body of the
function and the environment mapping the free variable @racket['x] to
8.

We can also confirm our larger example works:

@ex[
(interp
  '(((λ (t)
       ((λ (f) (t (λ (z) ((f f) z))))
        (λ (f) (t (λ (z) ((f f) z))))))
     (λ (tri)
       (λ (n)
         (if (zero? n)
             1
             (+ n (tri (sub1 n)))))))
    10))
]

While can't apply the interpretation of functions in Racket
like we did previously, we can @racket[apply-function] the
interpretation of functions:

@ex[
(define Y
  (interp
    '(λ (t)
       ((λ (f) (t (λ (z) ((f f) z))))
        (λ (f) (t (λ (z) ((f f) z))))))))

(define tri
  (interp '(λ (tri)
             (λ (n)
               (if (zero? n)
                   1
                   (+ n (tri (sub1 n))))))))

(apply-function (apply-function Y tri) 10)
]

The process we used to eliminate function values from the interpreter
is an instance of a general-purpose whole-program transformation
called @bold{defunctionalization} for replacing function values with
data structures.

@section[#:tag-prefix "loot"]{Defunctionalization at work}

Let's digress for a moment and learn this very useful transformation.

Here is a data type for representing regular expressions:

@#reader scribble/comment-reader
(racketblock
;; type Regexp =
;; | 'zero
;; | 'one
;; | `(char ,Char)
;; | `(times ,Regexp ,Regexp)
;; | `(plus ,Regexp ,Regexp)
;; | `(star ,Regexp)
)

The regular expression @racket['zero] matches nothing; @racket['one]
matches the empty string; @racket[`(char ,_c)] matches the character
@racket[_c]; @racket[`(times ,_r1 ,_r2)] matches the concatenation of
a string matching @racket[_r1] followed by a string matching
@racket[_r2]; @racket[`(plus ,_r1 ,_r2)] matching either a string
matching @racket[_r1] or a string matching @racket[_r2]; and
@racket[`(star ,_r)] matches a string made up of any number of
substrings, each of which match @racket[_r].

A really nice way to write a matcher is to use a continuation-passing
style that keeps track of what is required of the remainder of the
string after matching a prefix against the regexp:

@codeblock-include["loot/regexp.rkt"]

@(ev `(require (file ,(path->string (build-path notes "loot" "regexp.rkt")))))

Let's give it a try:
@ex[
(accepts `(star (char #\a)) "aaaaa")
(accepts `(star (char #\a)) "aaaab")
(accepts `(star (plus (char #\a) (char #\b))) "aaaab")
]
	
But what if needed to program this regular expression matching without
the use of function values?  We can arrive at such code systematically
by applying defunctionalization.

@codeblock-include["loot/regexp-defun.rkt"]

@(ev `(require (file ,(path->string (build-path notes "loot" "regexp-defun.rkt")))))


And we get the same results:

@ex[
(accepts `(star (char #\a)) "aaaaa")
(accepts `(star (char #\a)) "aaaab")
(accepts `(star (plus (char #\a) (char #\b))) "aaaab")
]


@section[#:tag-prefix "loot"]{Compiling Loot}

