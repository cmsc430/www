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

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "knock" f))))))
	   '("interp.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt"))

@title[#:tag "Knock"]{Knock: first-class function (pointers)}

@table-of-contents[]

@section[#:tag-prefix "knock"]{First-class function (pointers)}

With Iniquity and Jig, we have introduced functions and function
calls, but functions are second-class language mechanisms: functions
are not values.  They cannot be computed.  They cannot be stored in a
list or box.  They cannot be passed as arguments are returned as
results of other functions.

This is too bad since so many program designs depend on the idea of
computation-as-a-value at the heart of functional and object-oriented
programming.

Let's now remedy this problem by making functions first-class values
in our language.  We'll call it @bold{Knock}.


We add to the syntax two new forms, one for reference functions and
one for calling a function where the function position is an arbitrary
expression (that should evaluate to a function):

@verbatim|{
;; type Expr =
;; | ....
;; | `(fun ,Variable)
;; | `(call ,Expr ,@(Listof Expr))
}|

These new syntactic forms are temporary forms that don't correspond
anything in Racket but make it a bit easier to present how first-class
functions work.  The @racket[(fun _f)] form is a reference to a
function @racket[_f], which is defined in the program.  The
@racket[(call _e0 _es ...)] form is a function call where the function
position, @racket[_e0] is an arbitrary expression that should produce
a function value.

We will end up eliminating them in future versions of the compiler;
they are simply a crutch for now.


@section[#:tag-prefix "knock"]{A Compiler with Function pointers}

@codeblock-include["knock/compile.rkt"]

@ex[
(asm-interp
   (compile '(begin (define (f x)
                      (if (zero? x)
		          0
			  (add1 (call (fun f) (sub1 x)))))
                    (call (fun f) 10))))
]

@ex[
(asm-interp
   (compile '(begin (define (f x) (fun h))
                    (define (h y) y)
                    (call (call (fun f) 5) 9))))

]