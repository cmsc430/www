#lang racket
(provide (all-defined-out))

;; Expr -> Integer
(define (interp e)
  (match e
    [(? integer? i) i]
    [(? boolean? b) b]
    [`(add1 ,e0)
     (add1 (interp e0))]
    [`(sub1 ,e0)
     (sub1 (interp e0))]
    [`(zero? ,e0)
     (zero? (interp e0))]
    [`(if ,e0 ,e1 ,e2)
     (if (interp e0)
         (interp e1)
         (interp e2))]))
