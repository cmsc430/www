#lang racket
(provide Lit Prim1 If)
;; type Expr = (Lit Datum)
;;           | (Prim1 Op1 Expr)
;;           | (If Expr Expr Expr)
;; type Datum = Integer
;;            | Boolean
;;            | Character
;; type Op1 = 'add1 | 'sub1
;;          | 'zero?
;;          | 'char? | 'integer->char | 'char->integer
(struct Lit (d) #:prefab)
(struct Prim1 (p e) #:prefab)
(struct If (e1 e2 e3) #:prefab)

