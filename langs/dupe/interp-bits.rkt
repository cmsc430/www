#lang racket
(provide interp interp-bits)
(require "ast.rkt" "types.rkt" "interp-prim-bits.rkt")

;; Expr -> Value
(define (interp e)
  (bits->value (interp-bits e)))

;; Expr -> Bits
(define (interp-bits e)
  (match e
    [(Lit d) (value->bits d)]
    [(Prim1 p e)
     (interp-prim1-bits p (interp-bits e))]
    [(If e1 e2 e3)
     (if (= (interp-bits e1) (value->bits #f))
         (interp-bits e3)
         (interp-bits e2))]))
