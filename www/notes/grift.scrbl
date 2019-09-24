#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  "grift/semantics.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "grift" f))))))
	   '("interp.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt"))

@title[#:tag "Grift"]{Grift: binary operations}

@emph{If you have to eat two frogs, eat the ugliest one first.}

You may have noticed that up until this point, evaluating compound
expressions in our language always depend upon the result of a single
subexpression.  For example, @racket[(add1 _e)] depends upon the
result of @racket[_e], @racket[(zero? _e)] depends upon @racket[_e],
and so on.  Even expressions that involve multiple subexpressions such
as @racket[(if _e0 _e1 _e2)] really only depende on @racket[_e0] to
determine which of @racket[_e1] or @racket[_e2] to evaluate.

Let's now consider what happens when we have @bold{multiple
subexpressions} whose results must be combined in order to evaluate an
expression.  As an example, consider @racket[(+ _e0 _e1)].  We must
evaluate @emph{both} @racket[_e0] and @racket[_e1] and sum their
results.

We'll call this language @bold{Grift}.

What's new are the following @emph{binary} operations:

@racketblock[
(+ _e0 _e1)
(- _e0 _e1)
]

This leads to the following grammar for Grift:

@centered[(render-language G)]

We can model it as a datatype as usual:

@codeblock-include["grift/ast.rkt"]

@section{Meaning of Grift programs}

The meaning of Grift programs is pretty straightfoward.  For
@racket[(+ _e0 _e1)], the meaning is the sum of the meanings of
@racket[_e0] and @racket[_e1], when they mean integers, otherwise the
meaning is an error.



@(define ((rewrite s) lws)
   (define lhs (list-ref lws 2))
   (define rhs (list-ref lws 3))
   (list "" lhs (string-append " " (symbol->string s) " ") rhs ""))

@(require (only-in racket add-between))
@(define-syntax-rule (show-judgment name cases)
   (with-unquote-rewriter
      (lambda (lw)
        (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (lw-column lw) (lw-column-span lw)))
      (with-compound-rewriters (['+ (rewrite '+)]
                                ['- (rewrite '–)])
        (apply centered
	   (add-between 
             (map (λ (c) (parameterize ([judgment-form-cases (list c)]
	                                [judgment-form-show-rule-names #f])
	                   (render-judgment-form name)))
	          cases)
             (hspace 4))))))

The handling of primitives occurs in the following rule:

@(show-judgment 𝑮-𝒆𝒏𝒗 '("prim"))

It makes use of an auxiliary judgment for interpreting primitives:

@centered[
   (with-unquote-rewriter
      (lambda (lw)
        (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (lw-column lw) (lw-column-span lw)))
      (render-metafunction 𝑮-𝒑𝒓𝒊𝒎 #:contract? #t))]


The interpreter is likewise straightforward:

@codeblock-include["grift/interp.rkt"]

We can see that it works as expected:

@ex[
(interp '(+ 3 4))
(interp '(+ 3 (+ 2 2)))
(interp '(+ #f 8))
]

@section{A Compile for Grift}

Binary expressions are easy to deal with at the level of the semantics
and interpreter.  However things are more complicated at the level of
the compiler.

To see the problem consider blindly following the pattern we used:

@#reader scribble/comment-reader
(racketblock
;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 c)))
    `(,@c0 ; result in rax
      ,@c1 ; result in rax
      (add rax _???))))
)

The problem here is that executing @racket[c0] places its result in
register @racket['rax], but then executing @racket[c1] places its
result in @racket['rax], overwriting the value of @racket[e0].

It may be tempting to use another register to stash away the result of
the first subexpression:

@#reader scribble/comment-reader
(racketblock
;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 c)))
    `(,@c0 ; result in rax
      (mov rbx rax)
      ,@c1 ; result in rax
      (add rax rbx))))
)

Can you think of how this could go wrong?





@codeblock-include["grift/compile.rkt"]
