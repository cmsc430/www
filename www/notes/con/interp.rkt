#lang racket
(provide (all-defined-out))

;; Expr -> Integer
(define (con-interp e)
  (match e
    [(? integer? i) i]
    [`(add1 ,e0)
     (+ (con-interp e0) 1)]
    [`(sub1 ,e0)
     (- (con-interp e0) 1)]
    [`(if (zero? ,e0) ,e1 ,e2)
     (if (zero? (con-interp e0))
         (con-interp e1)
         (con-interp e2))]))
