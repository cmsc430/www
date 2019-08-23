#lang racket
(provide (all-defined-out))

;; type Answer = Value | 'err

;; Expr -> Answer
(define (interp e)
  (match e
    [(? integer? i) i]
    [(? boolean? b) b]
    [`(add1 ,e0)
     (match (interp e0)
       [(? integer? i) (add1 i)]
       [_ 'err])]
    [`(sub1 ,e0)
     (match (interp e0)
       [(? integer? i) (sub1 i)]
       [_ 'err])]
    [`(zero? ,e0)
     (match (interp e0)
       [(? integer? i) (zero? i)]
       [_ 'err])]
    [`(if ,e0 ,e1 ,e2)
     (match (interp e0)
       ['err 'err]
       [v
        (if v
            (interp e1)
            (interp e2))])]))
