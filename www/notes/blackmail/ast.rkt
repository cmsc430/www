#lang racket
(provide Int Add1 Sub1)

;; type Expr =
;; | (Int Integer)
;; | (Add1 Expr)
;; | (Sub1 Expr)
(struct Int (i) #:prefab)
(struct Add1 (e) #:prefab)
(struct Sub1 (e) #:prefab)
