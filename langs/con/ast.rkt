#lang racket
(require "../blackmail/ast.rkt")
(provide Int Prim1 IfZero)

;; type Expr = ...
;; | (IfZero Expr Expr Expr)
(struct IfZero (e1 e2 e3) #:prefab)
