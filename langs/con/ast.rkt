#lang racket
(provide Lit Prim1 IfZero)

;; type Expr =
;; | (Lit Integer)
;; | (Prim1 Op Expr)
;; | (IfZero Expr Expr Expr)
;; type Op = 'add1 | 'sub1
(struct Lit (i)           #:prefab)
(struct Prim1 (p e)       #:prefab)
(struct IfZero (e1 e2 e3) #:prefab)
