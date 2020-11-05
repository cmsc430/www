#lang racket
(provide Int Bool Add1 Sub1 If Zero?)

;; type Expr =
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Add1 Expr)
;; | (Sub1 Expr)
;; | (If Expr Expr Expr)
;; | (Zero? Expr)
(struct Int (i) #:prefab)
(struct Bool (b) #:prefab)
(struct Add1 (e) #:prefab)
(struct Sub1 (e) #:prefab)
(struct If (e1 e2 e3) #:prefab)
(struct Zero? (e) #:prefab)
