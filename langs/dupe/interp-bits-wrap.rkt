#lang racket
(provide interp-wrap interp-bits-wrap)
(require "ast.rkt")

;; type Value =
;; | Integer
;; | Boolean

(define word-size 64)

(define shift 1)
(define type-int  #b0)
(define val-true  #b01)
(define val-false #b11)

;; type Bits = Integer

;; Expr -> Bits
(define (interp-bits-wrap e)
  (match e
    [(Int i)  (arithmetic-shift i shift)]
    [(Bool b) (if b val-true val-false)]
    [(Prim 'add1 e0)
     (wrap (add1 (interp-bits-wrap e0)))]
    [(Prim 'sub1 e0)
     (wrap (sub1 (interp-bits-wrap e0)))]
    [(Prim 'zero? e)
     (if (zero? (interp-bits-wrap e))
         val-true
         val-false)]
    [(If e1 e2 e3)
     (if (= (interp-bits-wrap e1) val-false)
         (interp-bits-wrap e3)
         (interp-bits-wrap e2))]))

(define (interp-wrap e)
  (bits->value (interp-bits-wrap e)))

(define (bits->value b)
  (if (even? b)
      (arithmetic-shift b (- shift))
      (cond [(= b val-true)  #t]
            [(= b val-false) #f]
            [else (error "invalid bits")])))


(define (wrap n)
  (if (>= (integer-length n) (- word-size shift))
      (- (truncate n))
      n))

(define (truncate n)
  (bitwise-bit-field n
                     (max 0 (- (integer-length n)
                               (- word-size shift)))
                     (- word-size shift)))
