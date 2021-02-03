#lang racket
(provide Int Bool Char Prim1 If)

;; type Expr =
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Char Character)
;; | (Prim1 Op Expr)
;; | (If Expr Expr Expr)
;; type Op = 'add1 | 'sub1 | 'zero?
;;         | 'char? | 'integer->char | 'char->integer
(struct Int (i)       #:prefab)
(struct Bool (b)      #:prefab)
(struct Char (c)      #:prefab)
(struct Prim1 (p e)   #:prefab)
(struct If (e1 e2 e3) #:prefab)
