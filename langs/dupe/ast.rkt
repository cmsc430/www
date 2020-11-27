#lang racket
(provide Int Bool Prim If)

;; type Expr =
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Prim Op Expr)
;; | (If Expr Expr Expr)
;; type Op = 'add1 | 'sub1 | 'zero?
(struct Int (i) #:prefab)
(struct Bool (b) #:prefab)
(struct Prim (p e) #:prefab)
(struct If (e1 e2 e3) #:prefab)
