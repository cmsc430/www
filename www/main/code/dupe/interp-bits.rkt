#lang racket
(provide interp interp-bits)
(require "ast.rkt" "types.rkt")

;; Expr -> Value
(define (interp e)
  (bits->value (interp-bits e)))

;; Expr -> Bits
(define (interp-bits e)
  (match e
    [(Int i)  (value->bits i)]
    [(Bool b) (value->bits b)]
    [(Prim1 'add1 e0)
     (+ (interp-bits e0) (value->bits 1))]
    [(Prim1 'sub1 e0)
     (- (interp-bits e0) (value->bits 1))]
    [(Prim1 'zero? e)
     (if (zero? (interp-bits e))
         val-true
         val-false)]
    [(If e1 e2 e3)
     (if (= (interp-bits e1) val-false)
         (interp-bits e3)
         (interp-bits e2))]))

