#lang racket
(provide Lit Prim1 If)

;; type Expr =
;; | (Lit Datum)
;; | (Prim1 Op Expr)
;; | (If Expr Expr Expr)
;; type Datum = Boolean | Integer
;; type Op = 'add1 | 'sub1 | 'zero?

(struct Lit (d)       #:prefab)
(struct Prim1 (p e)   #:prefab)
(struct If (e1 e2 e3) #:prefab)
