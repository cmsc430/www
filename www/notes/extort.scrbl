#lang scribble/manual

@(require (for-label (except-in racket ... compile)))
@(require redex/pict
          racket/runtime-path
          scribble/examples
          "../fancyverb.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  extort/semantics
	  "../utils.rkt")



@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path langs "extort" f))))))
	   '("interp.rkt" "ast.rkt" "parse.rkt" "compile.rkt" "types.rkt"))

@(ev `(current-directory ,(path->string (build-path langs "extort"))))
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))

@(define this-lang "Extort")

@title[#:tag this-lang]{@|this-lang|: when errors exist}

@src-code[this-lang]

@emph{The greatest mistake is to imagine that we never err.}

@table-of-contents[]

@section[#:tag "errors"]{Errors}

We have added multiple, disjoint types, but mostly swept issues of
errors under the rug by considering type mismatches as meaningless.
Now let's redesign the semantics to specify the error behavior of such
programs.


We'll call it @bold{@this-lang}.

Nothing changes in the syntax of @this-lang from the previous
language, although we will need to talk about two kinds of
@emph{results} from evaluating programs: values and errors.  We will
say that evaluation produces an @bold{answer}, which is either a value
or error:

@centered{@render-language[E]}

@section{Meaning of @this-lang programs}

Languages adopt several approaches to type mismatches:

@itemlist[

@item{Prohibit such programs statically with a type system (e.g. OCaml, Java)}
@item{Coerce values to different types (e.g. JavaScript)}
@item{Signal a run-time error (e.g. Racket)}
@item{Leave the behavior unspecified (e.g. Scheme, C)}

]

We've previously seen the last approach.  Now let's do what Racket
does and signal an error.


The meaning of @this-lang programs that have type errors will now be
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
@codeblock-include["extort/interp-prim.rkt"]

We can confirm the interpreter computes the right result for the
examples given earlier:

@ex[
(interp (Prim1 'add1 (Lit #f)))
(interp (Prim1 'zero? (Lit #t)))
(interp (If (Prim1 'zero? (Lit #f)) (Lit 1) (Lit 2)))
]

The statement of correctness stays the same, but now observe that
there is no way to crash the interpreter with any @tt{Expr} value.


@section{A Compiler for @this-lang}

Suppose we want to compile @racket[(add1 #f)], what needs to happen?
Just as in the interpreter, we need to check the integerness of the
argument's value before doing the addition operation.

We extend the run-time system with a C function called
@tt{raise_error} that prints "err" and exits with a non-zero status to
indicate something has gone wrong.  @margin-note{The runtime system is
written with a level of indirection between @tt{raise_error} and the
code that prints and exits in @tt{error_exit}; this is done so that
the testing framework can intercede and replace the error function,
but it can be ignored.}

@filebox-include[fancy-c extort "main.c"]

Most of the work of error checking happens in the code emitted for
primitive operations.  Whenever an error is detected, control jumps to
a label called @racket['err] that immediately calls @tt{raise_error}:

@codeblock-include["extort/compile-ops.rkt"]

All that's left for the top-level compile function to declare an
external label @racket['raise_error] that will be defined by the
run-time system and to emit a label called @racket['err] that calls
@tt{raise_error}, otherwise this part of the compiler doesn't change:

@codeblock-include["extort/compile.rkt"]

Here's the code we generate for @racket['(add1 #f)]:
@ex[
(define (show e)
  (displayln (asm-string (compile-e (parse e)))))

(show '(add1 #f))
]

@(void (ev '(current-objs '("runtime.o"))))

Here are some examples running the compiler:
@ex[
(define (tell e)
  (match (asm-interp (compile (parse e)))
    ['err 'err]
    [b (bits->value b)]))
(tell #t)
(tell #f)
(tell '(zero? 0))
(tell '(zero? -7))
(tell '(if #t 1 2))
(tell '(if #f 1 2))
(tell '(if (zero? 0) (if (zero? 0) 8 9) 2))
(tell '(if (zero? (if (zero? 2) 1 0)) 4 5))
(tell '(add1 #t))
(tell '(sub1 (add1 #f)))
(tell '(if (zero? #t) 1 2))
]

Since the interpreter and compiler have well defined specifications
for what should happen when type errors occur, we can test in the
usual way again:

@ex[
(define (check-correctness e)
  (check-equal? (match (asm-interp (compile e))
                  ['err 'err]
                  [b (bits->value b)])
                (interp e)
                e))

(check-correctness (Prim1 'add1 (Lit 7)))
(check-correctness (Prim1 'add1 (Lit #f)))
]
