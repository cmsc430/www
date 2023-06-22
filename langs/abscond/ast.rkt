#lang racket
(provide Lit)

;; type Expr = (Lit Integer)
(struct Lit (i) #:prefab)
