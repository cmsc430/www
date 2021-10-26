#lang racket
(provide (all-defined-out))

(define imm-shift          3)
(define imm-mask       #b111)
(define ptr-mask       #b111)
(define type-box       #b001)
(define type-cons      #b010)
(define type-vect      #b011)
(define type-str       #b100)
(define type-proc      #b101)
(define int-shift  (+ 1 imm-shift))
(define char-shift (+ 2 imm-shift))
(define type-int      #b0000)
(define mask-int      #b1111)
(define type-char    #b01000)
(define mask-char    #b11111)
(define val-true   #b0011000)
(define val-false  #b0111000)
(define val-eof    #b1011000)
(define val-void   #b1111000)
(define val-empty #b10011000)

(define (bits->value b)
  (cond [(= type-int (bitwise-and b mask-int))
         (arithmetic-shift b (- int-shift))]
        [(= type-char (bitwise-and b mask-char))
         (integer->char (arithmetic-shift b (- char-shift)))]
        [(= b val-true)  #t]
        [(= b val-false) #f]
        [(= b val-eof)  eof]
        [(= b val-void) (void)]
        [(= b val-empty) '()]
        [else (error "invalid bits")]))

(define (imm->bits v)
  (cond [(eof-object? v) val-eof]
        [(integer? v) (arithmetic-shift v int-shift)]
        [(char? v)
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]
        [(eq? v #t) val-true]
        [(eq? v #f) val-false]
        [(void? v)  val-void]
        [(empty? v) val-empty]))


(define (imm-bits? v)
  (zero? (bitwise-and v imm-mask)))

(define (int-bits? v)
  (zero? (bitwise-and v mask-int)))

(define (char-bits? v)
  (= type-char (bitwise-and v mask-char)))

(define (cons-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-cons)))

(define (box-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-box)))

(define (vect-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-vect)))

(define (str-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-str)))

(define (proc-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-proc)))
