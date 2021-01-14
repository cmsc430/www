#lang racket
(provide Int)

;; type Expr = (Int Integer)
(struct Int (i) #:prefab)
