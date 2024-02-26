#lang racket
(provide Lit Prim1)

;; type Expr = (Lit Integer)
;;           | (Prim1 Op1 Expr)
;; type Op1 = 'add1 | 'sub1
(struct Lit (i) #:prefab)
(struct Prim1 (p e) #:prefab)

