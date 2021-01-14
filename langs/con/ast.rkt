#lang racket
(provide Int Prim IfZero)

;; type Expr =
;; | (Int Integer)
;; | (Prim Op Expr)
;; | (IfZero Expr Expr Expr)
;; type Op = 'add1 | 'sub1
(struct Int (i) #:prefab)
(struct Prim (p e) #:prefab)
(struct IfZero (e1 e2 e3) #:prefab)
