#lang racket
(provide (all-defined-out))

;; Expr -> Integer
(define (interp e)
  (match e
    [(? integer? i) i]
    [`(add1 ,e0)
     (+ (interp e0) 1)]
    [`(sub1 ,e0)
     (- (interp e0) 1)]
    [`(if (zero? ,e0) ,e1 ,e2)
     (if (zero? (interp e0))
         (interp e1)
         (interp e2))]))
