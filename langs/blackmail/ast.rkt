#lang racket
(provide Int Prim)

;; type Expr =
;; | (Int Integer)
;; | (Prim Op Expr)
;; type Op = 'add1 | 'sub1
(struct Int (i) #:prefab)
(struct Prim (p e) #:prefab)
