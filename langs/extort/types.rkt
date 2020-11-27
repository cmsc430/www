#lang racket
(provide (all-defined-out))

(define int-shift      1)
(define char-shift     2)
(define type-int     #b0)
(define mask-int     #b1)
(define type-char   #b01)
(define mask-char   #b11)
(define val-true  #b0011)
(define val-false #b0111)
(define val-eof   #b1011)
(define val-void  #b1111)

(define (bits->value b)
  (cond [(= type-int (bitwise-and b #b1))
         (arithmetic-shift b (- int-shift))]
        [(= type-char (bitwise-and b #b11))
         (integer->char (arithmetic-shift b (- char-shift)))]
        [(= b val-true)  #t]
        [(= b val-false) #f]
        [(= b val-eof)  eof]
        [(= b val-void) (void)]
        [else (error "invalid bits")]))

(define (value->bits v)
  (cond [(eof-object? v) val-eof]
        [(integer? v) (arithmetic-shift v int-shift)]
        [(char? v)
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]
        [(eq? v #t) val-true]
        [(eq? v #f) val-false]
        [(void? v)  val-void]))

