#lang racket
(provide Int Prim1 IfZero)

;; type Expr =
;; | (Int Integer)
;; | (Prim1 Op Expr)
;; | (IfZero Expr Expr Expr)
;; type Op = 'add1 | 'sub1
(struct Int (i)           #:prefab)
(struct Prim1 (p e)       #:prefab)
(struct IfZero (e1 e2 e3) #:prefab)
