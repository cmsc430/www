#lang racket
(provide (all-defined-out))
(define int-shift    1)
(define mask-int   #b1)
(define type-int   #b0)

(define (bits->value b)
  (cond [(= b (value->bits #t))  #t]
        [(= b (value->bits #f)) #f]
        [(int-bits? b)
         (arithmetic-shift b (- int-shift))]
        [else (error "invalid bits")]))

(define (value->bits v)
  (cond [(eq? v #t) #b01]
        [(eq? v #f) #b11]
        [(integer? v) (arithmetic-shift v int-shift)]))

(define (int-bits? v)
  (= type-int (bitwise-and v mask-int)))

