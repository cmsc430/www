#lang racket
(provide (all-defined-out))
(require (only-in "interp.rkt" prim? value? interp-prim)
         "translate.rkt")

;; type VEnv = (Listof Value)

;; Expr -> Answer
(define (interp e)
  (interp-env (translate e) '()))

;; IExpr VEnv -> Answer
(define (interp-env e r)
  (match e
    [(? value? v) v]
    [(list (? prim? p) e)
     (let ((a (interp-env e r)))
       (interp-prim p a))]
    [`(if ,e0 ,e1 ,e2)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [`(address ,i)
     (list-ref r i)]
    [`(let ((_ ,e0)) ,e1)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (interp-env e1 (cons v r))])]))
