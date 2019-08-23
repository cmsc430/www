#lang racket
(provide (all-defined-out))

;; Expr (Value -> Answer) -> Answer
(define (interp/cps e k)
  (match e
    [(? integer? i) (k i)]
    [(? boolean? b) (k b)]
    [`(add1 ,e0)   
     (interp/cps e0 (assert integer? add1))]
    [`(sub1 ,e0)
     (interp/cps e0 (assert integer? sub1))]
    [`(zero? ,e0)
     (interp/cps e0 (assert integer? zero?))]
     [`(if ,e0 ,e1 ,e2)
      (interp/cps e0 (λ (v)
                       (if v
                           (interp e1)
                           (interp e2))))]))

;; (Value -> Boolean) (Value -> Answer) -> (Value -> Answer)
(define (assert pred k)
  (λ (v)
    (if (pred v)
        (k v)
        'error)))
