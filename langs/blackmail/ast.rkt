#lang racket
(require "../abscond/ast.rkt")
(provide Int Prim1)

;; type Expr = ...
;; | (Prim1 Op Expr)
;; type Op = 'add1 | 'sub1
(struct Prim1 (p e) #:prefab)
