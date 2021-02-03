#lang racket
(provide Int Prim1)

;; type Expr =
;; | (Int Integer)
;; | (Prim1 Op Expr)
;; type Op = 'add1 | 'sub1
(struct Int (i)     #:prefab)
(struct Prim1 (p e) #:prefab)
