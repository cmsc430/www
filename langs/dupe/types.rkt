#lang racket
(provide (all-defined-out))

(define int-shift    1)
(define type-int   #b0)

(define (bits->value b)
  (cond [(= b (value->bits #t))  #t]
        [(= b (value->bits #f)) #f]
        [(= type-int (bitwise-and b #b1))
         (arithmetic-shift b (- int-shift))]
        [else (error "invalid bits")]))

(define (value->bits v)
  (cond [(eq? v #t) #b011]
        [(eq? v #f) #b111]
        [(integer? v) (arithmetic-shift v int-shift)]))
