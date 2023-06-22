#lang racket
(provide Lit Prim1)

;; type Expr =
;; | (Lit Integer)
;; | (Prim1 Op Expr)
;; type Op = 'add1 | 'sub1
(struct Lit (i)     #:prefab)
(struct Prim1 (p e) #:prefab)
