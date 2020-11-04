#lang racket
(provide Int Add1 Sub1 IfZero)

;; type Expr =
;; | (Int Integer)
;; | (Add1 Expr)
;; | (Sub1 Expr)
;; | (IfZero Expr Expr Expr)
(struct Int (i) #:prefab)
(struct Add1 (e) #:prefab)
(struct Sub1 (e) #:prefab)
(struct IfZero (e1 e2 e3) #:prefab)
