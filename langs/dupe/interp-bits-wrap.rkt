#lang racket
(provide interp-wrap interp-bits-wrap)
(require "ast.rkt" "types.rkt")

;; type Value =
;; | Integer
;; | Boolean

(define word-size 64)

(define shift 1)

;; type Bits = Integer

;; Expr -> Bits
(define (interp-bits-wrap e)
  (match e
    [(Lit i)  (value->bits i)]
    [(Prim1 'add1 e0)
     (wrap (add1 (interp-bits-wrap e0)))]
    [(Prim1 'sub1 e0)
     (wrap (sub1 (interp-bits-wrap e0)))]
    [(Prim1 'zero? e)
     (value->bits (zero? (interp-bits-wrap e)))]
    [(If e1 e2 e3)
     (if (= (interp-bits-wrap e1) (value->bits #f))
         (interp-bits-wrap e3)
         (interp-bits-wrap e2))]))

(define (interp-wrap e)
  (bits->value (interp-bits-wrap e)))

(define (wrap n)
  (if (>= (integer-length n) (- word-size shift))
      (- (truncate n))
      n))

(define (truncate n)
  (bitwise-bit-field n
                     (max 0 (- (integer-length n)
                               (- word-size shift)))
                     (- word-size shift)))
