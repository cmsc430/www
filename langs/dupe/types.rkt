#lang racket
(provide (all-defined-out))

(define int-shift    1)
(define type-int   #b0)
(define val-true  #b01)
(define val-false #b11)

(define (bits->value b)
  (cond [(= type-int (bitwise-and b #b1))         
         (arithmetic-shift b (- int-shift))]
        [(= b val-true)  #t]
        [(= b val-false) #f]
        [else (error "invalid bits")]))

(define (value->bits v)
  (cond [(integer? v) (arithmetic-shift v int-shift)]
        [(eq? v #t) val-true]
        [(eq? v #f) val-false]))
