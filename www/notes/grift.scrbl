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
	   '("interp.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt" "ast.rkt" "parse.rkt"))

@title[#:tag "Grift"]{Grift: binary operations}

@emph{If you have to eat two frogs, eat the ugliest one first.}

You may have noticed that up until this point, evaluating compound
expressions in our language always depend upon the result of a single
subexpression.  For example, @racket[(add1 _e)] depends upon the
result of @racket[_e], @racket[(zero? _e)] depends upon @racket[_e],
and so on.  Even expressions that involve multiple subexpressions such
as @racket[(if _e0 _e1 _e2)] really only depends on @racket[_e0] to
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

The meaning of Grift programs is pretty straightforward.  For
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
(interp (parse '(+ 3 4)))
(interp (parse '(+ 3 (+ 2 2))))
(interp (parse '(+ #f 8)))
]

@section{A Compile for Grift}

Binary expressions are easy to deal with at the level of the semantics
and interpreter.  However things are more complicated at the level of
the compiler.

To see the problem consider blindly following the pattern we used (and
ignoring type errors for the moment):

@#reader scribble/comment-reader
(racketblock
;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (append (compile-e e0 c)
          (compile-e e1 c)
          (list (Add 'rax _????))))
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
  (append (compile-e e0 c)
          (list (Mov 'rbx 'rax))
          (compile-e e1 c)
          (list (Add 'rax 'rbx))))
)

Can you think of how this could go wrong?

To come up with a general solution to this problem, we need to save
the result of @racket[_e0] and then retrieve it after computing
@racket[_e1] and it's time to sum.

Note that this issue only comes up when @racket[_e0] is a
@bold{serious} expression, i.e. an expression that must do some
computation.  If @racket[_e0] were a literal integer or a variable, we
could emit working code.  For example:


@#reader scribble/comment-reader
(racketblock
;; Integer Expr CEnv -> Asm
;; A special case for compiling (+ i0 e1)
(define (compile-+-int i0 e1 c)
  (append (compile-e e1 c)
          (list (Add 'rax (arithmetic-shift i0 imm-shift)))))

;; Id Expr CEnv -> Asm
;; A special case for compiling (+ x0 e1)
(define (compile-+-var x0 e1)
  (let ((i (lookup x0 c)))
    (append (compile-e e1 c)   
            (list (Add 'rax (Offset 'rsp (- (add1 i))))))))
)

The latter suggests a general solution could be to transform binary
primitive applications into a @racket[let] form that binds the first
subexpression to a variable and then uses the @racket[compile-+-var]
function above.  The idea is that every time the compiler encounters
@racket[(+ _e0 _e1)], we transform it to @racket[(let ((_x _e0)) (+ _x
_e1))].  For this to work out, @racket[_x] needs to be some variable
that doesn't appear free in @racket[_e1].  This transformation is
what's called @bold{ANF} (administrative normal form) and is a widely
used intermediate representation for compilers.


But, we can also solve the problem more directly by considering the
code that is generated for the ANF style expression above.

Consider the lexical address of @racket[_x] in the transformed code
above.  It is @emph{always} 0 because the transformation puts the
@racket[let] immediately around the occurrence of @racket[_x].  So if
we're compiling @racket[(+ _e0 _e1)] in environment @racket[_c] using
this approach, we know the value of @racket[_e0] will live at
@racket[`(offset rsp ,(- (add1 (length c))))].  There's no need for a
@racket[let] binding or a fresh variable name.  And this observation
enables us to write a general purpose compiler for binary primitives
that doesn't require any program transformation: we simply push the
value of @racket[e0] on the top of the stack and retrieve it later.

Here is a first cut:

@#reader scribble/comment-reader
(racketblock
;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (let ((x (gensym))) ; generate a fresh variable
    (append (compile-e e0 c)
            (list (Mov (Offset 'rsp (add1 (- (length c)))) 'rax))
            (compile-e e1 (cons x c))
            (list (Add 'rax (Offset 'rsp (- (add1 (lookup x (cons x c))))))))))
)

There are a couple things to notice.  First: the @racket[(lookup x
(cons x c))] just produces @racket[(length c)].  Second, when
compiling @racket[_e1] in environment @racket[(cons x c)], we know
that no variable in @racket[_e1] resolves to @racket[x] because
@racket[x] is a freshly @racket[gensym]'d symbol.  Putting (an
unreferenced) @racket[x] in the environment serves only to ``bump up''
by one the offset of any variable bound after @racket[x] so as to not
override the spot where @racket[e0]'s values lives.  We can accomplish
the same thing by sticking in something that no variable is equal to:
@racket[#f]:

@#reader scribble/comment-reader
(racketblock
;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (append (compile-e e0 c)
          (list (Mov (Offset 'rsp (add1 (- (length c)))) 'rax))
          (compile-e e1 (cons #f c))
          (list (Add 'rax (Offset 'rsp (- (add1 (length c))))))))
)

The complete code for the compiler is:


@codeblock-include["grift/compile.rkt"]
