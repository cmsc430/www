#lang racket
(provide (all-defined-out))
(define int-shift    1)
(define mask-int   #b1)
(define char-shift   2)
(define type-int   #b0)
(define type-char #b01)
(define mask-char #b11)

(define (bits->value b)
  (cond [(= b (value->bits #t))  #t]
        [(= b (value->bits #f)) #f]
        [(= b (value->bits eof))  eof]
        [(= b (value->bits (void))) (void)]
        [(int-bits? b)
         (arithmetic-shift b (- int-shift))]
        [(char-bits? b)
         (integer->char (arithmetic-shift b (- char-shift)))]
        [else (error "invalid bits")]))

(define (value->bits v)
  (cond [(eq? v #t) #b011]
        [(eq? v #f) #b111]
        [(integer? v) (arithmetic-shift v int-shift)]
        [(eof-object? v) #b1011]
        [(void? v)       #b1111]
        [(char? v)
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]))

(define (int-bits? v)
  (= type-int (bitwise-and v mask-int)))

(define (char-bits? v)
  (= type-char (bitwise-and v mask-char)))

