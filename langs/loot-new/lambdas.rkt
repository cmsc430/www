#lang racket
(require "ast.rkt")
(provide lambdas)

;; Expr -> [Listof Lam]
;; List all of the lambda expressions in e
(define (lambdas e)
  (match e
    [(Prim1 p e)        (lambdas e)]
    [(Prim2 p e1 e2)    (append (lambdas e1) (lambdas e2))]
    [(Prim3 p e1 e2 e3) (append (lambdas e1) (lambdas e2) (lambdas e3))]
    [(If e1 e2 e3)      (append (lambdas e1) (lambdas e2) (lambdas e3))]
    [(Begin e1 e2)      (append (lambdas e1) (lambdas e2))]
    [(Let x e1 e2)      (append (lambdas e1) (lambdas e2))]
    [(App e1 es)        (append (lambdas e1) (append-map lambdas es))]
    [(Lam f xs e1)      (cons e (lambdas e1))]
    [_                  '()]))
