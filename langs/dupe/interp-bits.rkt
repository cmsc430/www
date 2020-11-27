#lang racket
(provide interp interp-bits)
(require "ast.rkt")

;; type Value =
;; | Integer
;; | Boolean

(define int-shift    1)
(define type-int   #b0)
(define val-true  #b01)
(define val-false #b11)

;; type Bits = Integer

;; Expr -> Bits
(define (interp-bits e)
  (match e
    [(Int i)  (arithmetic-shift i int-shift)]
    [(Bool b) (if b val-true val-false)]    
    [(Prim 'add1 e0)
     (+ (interp-bits e0) (arithmetic-shift 1 int-shift))]
    [(Prim 'sub1 e0)
     (- (interp-bits e0) (arithmetic-shift 1 int-shift))]
    [(Prim 'zero? e)
     (if (zero? (interp-bits e))
         val-true
         val-false)]
    [(If e1 e2 e3)
     (if (= (interp-bits e1) val-false)
         (interp-bits e3)
         (interp-bits e2))]))

(define (interp e)
  (bits->value (interp-bits e)))

(define (bits->value b)
  (cond [(= type-int (bitwise-and b #b1))
         (arithmetic-shift b (- int-shift))]     
        [(= b val-true)  #t]
        [(= b val-false) #f]
        [else (error "invalid bits")]))
