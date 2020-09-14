#lang racket
(provide (all-defined-out))

; type Expr = integer
;          | get-int
(struct int-e (i))
(struct get-i ())

(define (print-ast a)
  (match a
    [(int-e i) `(int-e ,i)]
    [(get-i)    'get-i]))
