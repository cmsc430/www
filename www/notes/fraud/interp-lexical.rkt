#lang racket
(provide (all-defined-out))
(require (only-in "interp.rkt" prim? value? interp-prim))

;; Expr -> IExpr
(define (translate e)
  (translate-e e '()))

;; Expr LEnv -> IExpr
(define (translate-e e r)
  (match e
    [(? value? v) v]
    [(list (? prim? p) e)
     (list p (translate-e e r))]
    [`(if ,e0 ,e1 ,e2)
     `(if ,(translate-e e0)
          ,(translate-e e1)
	  ,(translate-e e2))]
    [(? symbol? x)
     (lexical-address x r)]
    [`(let ((,x ,e0)) ,e1)
     `(let ((_ ,(translate e0 r)))
       ,(translate e1 (cons x r)))]))

;; Variable LEnv -> Natural
(define (lexical-address x r)
  (match r
    ['() (error "unbound variable")]
    [(cons y r)
     (match (symbol=? x y)
       [#t (length r)]
       [#f (lexical-address x r)])]))
