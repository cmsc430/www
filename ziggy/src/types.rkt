#lang crook
{:= D0 D1}
(provide (all-defined-out))

(define int-shift    1)
(define mask-int   #b1)
{:> D1} (define char-shift   2)
(define type-int   #b0)
{:> D1} (define type-char #b01)
{:> D1} (define mask-char #b11)

(define (bits->value b)
  (cond [(= b (value->bits #t))  #t]
        [(= b (value->bits #f)) #f]
        [(int-bits? b)
         (arithmetic-shift b (- int-shift))]
        {:> D1}
        [(char-bits? b)
         (integer->char (arithmetic-shift b (- char-shift)))]
        [else (error "invalid bits")]))

(define (value->bits v)
  (cond [(eq? v #t) #b011]
        [(eq? v #f) #b111]
        [(integer? v) (arithmetic-shift v int-shift)]
        {:> D1}
        [(char? v)
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]))

(define (int-bits? v)
  (= type-int (bitwise-and v mask-int)))

{:> D1}
(define (char-bits? v)
  (= type-char (bitwise-and v mask-char)))
