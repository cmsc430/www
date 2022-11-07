#lang racket
(provide max-stack)
(require "ast.rkt")

;; Expr -> Natural
(define (max-stack e)
  (match e
    [(Int i)            0]
    [(Bool b)           0]
    [(Char c)           0]
    [(Eof)              0]
    [(Empty)            0]
    [(Var x)            0]
    [(Str s)            0]
    [(Prim0 p)          0]
    [(Prim1 p e)        (max-stack e)]
    [(Prim2 p e1 e2)    (max (max-stack e1) (add1 (max-stack e2)))]
    [(Prim3 p e1 e2 e3) (max (max-stack e1) (add1 (max-stack e2)) (+ 2 (max-stack e3)))]
    [(If e1 e2 e3)      (max (max-stack e1) (max-stack e2) (max-stack e3))]
    [(Begin e1 e2)      (max (max-stack e1) (max-stack e2))]
    [(Let x e1 e2)      (max (max-stack e1) (add1 (max-stack e2)))]
    [(App f es)         (add1 (max-stack-es es))]))

;; [Listof Expr] -> Natural
(define (max-stack-es es)
  (match es
    ['() 0]
    [(cons e es)
     (max (max-stack e) (add1 (max-stack-es es)))]))


;(max-stack (App 'f (list (Int 0) (Int 1) (Int 2) (Int 3))))
