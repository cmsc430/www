#lang racket
(provide Int Bool If Prim Let Var)

;; type Expr =
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Prim Op Expr)
;; | (If Expr Expr Expr)
;; | (Let Id Expr Expr)
;; | (Var Id)
;; type Id = Symbol
;; type Op = 'add1 | 'sub1 | 'zero?
(struct Int (i) #:prefab)
(struct Bool (b) #:prefab)
(struct Prim (p e) #:prefab)
(struct If (e1 e2 e3) #:prefab)
(struct Let (x e1 e2) #:prefab)
(struct Var (x) #:prefab)
