#lang racket
(provide (all-defined-out))
(require "ast.rkt"
         (only-in "interp.rkt" interp-prim))

;; type LEnv = (Listof Variable)

;; Expr -> IExpr
(define (translate e)
  (translate-e e '()))

;; Expr LEnv -> IExpr
(define (translate-e e r)
  (match e
    [(var-e v)
     (address-e (lexical-address v r))]
    [(? value? v) v]
    [(prim-e (? prim? p) e)
     (prim-e p (translate-e e r))]
    [(if-e e0 e1 e2)
     (if-e (translate-e e0 r)
           (translate-e e1 r)
           (translate-e e2 r))]
    [(let-e (list (binding x def)) body)
     ; we use '_ below because we don't care what it's called
     (let-e (list (binding '_ (translate-e def r)))
       (translate-e body (cons x r)))]))

;; Variable LEnv -> Natural
(define (lexical-address x r)
  (match r
    ['() (error "unbound variable")]
    [(cons y r)
     (match (symbol=? x y)
       [#t 0]
       [#f (add1 (lexical-address x r))])]))
