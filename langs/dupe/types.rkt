#lang racket
(provide (all-defined-out))

;; type Value =
;; | Integer
;; | Boolean

;; type Bits = Integer

(define int-shift    1)
(define type-int   #b0)
(define type-bool  #b1)
(define val-true  #b01)
(define val-false #b11)

;; Bits -> Value
(define (bits->value b)
  (cond [(= type-int (bitwise-and b #b1))
         (arithmetic-shift b (- int-shift))]
        [(= b val-true)  #t]
        [(= b val-false) #f]
        [else (error "invalid bits")]))

;; Value -> Bits
(define (value->bits v)
  (match v
    [(? integer?) (arithmetic-shift v int-shift)]
    [#t val-true]
    [#f val-false]))
