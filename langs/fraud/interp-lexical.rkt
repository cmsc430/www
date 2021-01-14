#lang racket
(provide (all-defined-out))
(require "ast.rkt" "translate.rkt")

;; type VEnv = (Listof Value)

;; Expr -> Answer
(define (interp e)
  (interp-env (translate e) '()))

;; IExpr VEnv -> Answer
(define (interp-env e r)
  (match e
    [(Var a) (list-ref r a)]
    [(Int i) i]
    [(Bool b) b]
    [(Prim1 p e) (interp-prim p (interp-env e r))]
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Let '_ e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v (interp-env e2 (cons v r))])]))

;; Op Answer -> Answer
(define (interp-prim p a)
  (match a
    [(? integer? i)
     ((match p
        ['add1 add1]
        ['sub1 sub1]
        ['zero? zero?])
      i)]
    [_ 'err]))
