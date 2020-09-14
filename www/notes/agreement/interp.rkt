#lang racket
(provide (all-defined-out))

(require "primitives.rkt")
(require "ast.rkt")

; Expr -> Integer
; Interpret given expression
;
(define (interp p)
  (match p
    [(int-e i) i]
    [(get-i)   (get-int)]))
