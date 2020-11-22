#lang racket
(provide interp interp-bits)
(require "ast.rkt")

;; type Value =
;; | Integer
;; | Boolean

(define shift 1)
(define type-int  #b0)
(define val-true  #b01)
(define val-false #b11)

;; type Bits = Integer

;; Expr -> Bits
(define (interp-bits e)
  (match e
    [(Int i)  (arithmetic-shift i shift)]
    [(Bool b) (if b val-true val-false)]
    [(Prim 'add1 e0)
     (add1 (interp-bits e0))]
    [(Prim 'sub1 e0)
     (sub1 (interp-bits e0))]
    [(Prim 'zero? e)
     (zero? (interp-bits e))]
    [(If e1 e2 e3)
     (if (= (interp-bits e1) val-false)
         (interp-bits e3)
         (interp-bits e2))]))

(define (interp e)
  (bits->value (interp-bits e)))

(define (bits->value b)
  (if (even? b)
      (arithmetic-shift b (- shift))
      (cond [(= b val-true)  #t]
            [(= b val-false) #f]
            [else (error "invalid bits")])))
