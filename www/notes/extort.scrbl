#lang scribble/manual

@(require (for-label (except-in racket ... compile)))
@(require redex/pict
          racket/runtime-path
          scribble/examples
          "../fancyverb.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  "extort/semantics.rkt"
	  "../utils.rkt")



@(define codeblock-include (make-codeblock-include #'h))

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "extort" f))))))
	   '("interp.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt"))


@title[#:tag "Extort"]{Extort: when errors exist}

We have added multiple, disjoint types, but mostly swept issues of
errors under the rug by considering type mismatches as meaningless.
Now let's redesign the semantics to specify the error behavior of such
programs.


We'll call it @bold{Extort}.

Nothing changes in the syntax of Extort from Dupe, although we will
need to talk about two kinds of @emph{results} from evaluating
programs: values and errors.  We will say that evaluation produces an
@bold{answer}, which is either a value or error:

@centered{@render-language[E]}

@section{Meaning of Extort programs}

Languages adopt several approaches to type mismatches:

@itemlist[

@item{Prohibit such programs statically with a type system (e.g. OCaml, Java)}
@item{Coerce values to different types (e.g. JavaScript)}
@item{Signal a run-time error (e.g. Racket)}
@item{Leave the behavior unspecified (e.g. Scheme, C)}

]

We've previously seen the last approach.  Now let's do what Racket
does and signal an error.


The meaning of Extort programs that have type errors will now be
defined as @racket['err]:

@itemlist[

@item{@racket[(add1 #f)]: means @racket['err].}

@item{@racket[(zero? #t)]: means @racket['err].}

@item{@racket[(if (zero? #f) 1 2)]: means @racket['err].}

]

@(define ((rewrite s) lws)
   (define lhs (list-ref lws 2))
   (define rhs (list-ref lws 3))
   (list "" lhs (string-append " " (symbol->string s) " ") rhs ""))

@(require (only-in racket add-between))
@(define-syntax-rule (show-judgment name i j)
   (with-unquote-rewriter
      (lambda (lw)
        (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (lw-column lw) (lw-column-span lw)))
      (with-compound-rewriters (['+ (rewrite '+)]
                                ['- (rewrite '–)]
                                ['= (rewrite '=)]
				['!= (rewrite '≠)])
        (apply centered
	   (add-between 
             (build-list (- j i)
	                 (λ (n) (begin (judgment-form-cases (list (+ n i)))
	                               (render-judgment-form name))))
             (hspace 4))))))

There are three ways in which an error can be introduced:
@(show-judgment 𝑬 0 3)

And there are four rules for propagating errors from subexpressions:
@(show-judgment 𝑬 3 7)



Now what does the semantics say about @racket[(add1 #f)]?  What about
@racket[(if 7 #t -2)]?


The signature of the interpreter is extended to produce answers.  Each
use of a Racket primitive is guarded by checking the type of the
arguments and an error is produced if the check fails.  Errors are
also propagated when a subexpression produces an error:

@codeblock-include["extort/interp.rkt"]

We can confirm the interpreter computes the right result for the
examples given earlier:

@ex[
(interp '(add1 #f))
(interp '(zero? #t))
(interp '(if (zero? #f) 1 2))
]

The statement of correctness stays the same, but now observe that
there is no way to crash the interpreter with any @tt{Expr} value.


@section{An Example of Extort compilation}

Suppose we want to compile @racket[(add1 #f)], what needs to happen?
Just as in the interpreter, we need to check the integerness of the
argument's value before doing the addition operation.

We extend the run-time system with a C function called @tt{error}
that prints "err" and exits:

@filebox-include[fancy-c "extort/main.c"]

The compiler now emits code to check the type of arguments:

@codeblock-include["extort/compile.rkt"]

Here's the code we generate for @racket['(add1 #f)]:
@ex[
(asm-display (compile '(add1 #f)))
]

Here are some examples running the compiler:
@ex[
(asm-interp (compile #t))
(asm-interp (compile #f))
(asm-interp (compile '(zero? 0)))
(asm-interp (compile '(zero? -7)))
(asm-interp (compile '(if #t 1 2)))
(asm-interp (compile '(if #f 1 2)))
(asm-interp (compile '(if (zero? 0) (if (zero? 0) 8 9) 2)))
(asm-interp (compile '(if (zero? (if (zero? 2) 1 0)) 4 5)))
(asm-interp (compile '(add1 #t)))
(asm-interp (compile '(sub1 (add1 #f))))
(asm-interp (compile '(if (zero? #t) 1 2)))
]

Since the interpreter and compiler have well defined specifications
for what should happen when type errors occur, we can test in the
usual way again:

@ex[
(define (check-correctness e)
  (check-equal? (asm-interp (compile e))
                (interp e)
                e))

(check-correctness '(add1 7))
(check-correctness '(add1 #f))
]



