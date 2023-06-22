#lang racket
(provide (all-defined-out))

(define int-shift    1)
(define char-shift   2)
(define type-int   #b0)
(define type-char #b01)
(define mask-char #b11)

(define (bits->value b)
  (cond [(= b (value->bits #t))  #t]
        [(= b (value->bits #f)) #f]
        [(= type-int (bitwise-and b #b1))
         (arithmetic-shift b (- int-shift))]
        [(= type-char (bitwise-and b #b11))
         (integer->char (arithmetic-shift b (- char-shift)))]
        [else (error "invalid bits")]))

(define (value->bits v)
  (cond [(eq? v #t) #b011]
        [(eq? v #f) #b111]
        [(integer? v) (arithmetic-shift v int-shift)]
        [(char? v)
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]))
