#lang racket
(provide (all-defined-out))
(require ffi/unsafe)

(define imm-shift          3)
(define imm-mask       #b111)
(define ptr-mask       #b111)
(define type-box       #b001)
(define type-cons      #b010)
(define type-vect      #b011)
(define type-str       #b100)
(define int-shift (+ 1 imm-shift))
(define mask-int #b1111)
(define char-shift (+ 2 imm-shift))
(define type-int #b0000)
(define type-char #b01000)
(define mask-char #b11111)

(define (bits->value b)
  (cond [(= b (value->bits #t))  #t]
        [(= b (value->bits #f)) #f]
        [(= b (value->bits eof))  eof]
        [(= b (value->bits (void))) (void)]
        [(= b (value->bits '())) '()]
        [(int-bits? b)
         (arithmetic-shift b (- int-shift))]
        [(char-bits? b)
         (integer->char (arithmetic-shift b (- char-shift)))]
        [(box-bits? b)
         (box (bits->value (heap-ref b)))]
        [(cons-bits? b)
         (cons (bits->value (heap-ref (+ b 8)))
               (bits->value (heap-ref b)))]
        [(vect-bits? b)
         (if (zero? (untag b))
             (vector)
             (build-vector (heap-ref b)
                           (lambda (j)
                             (bits->value (heap-ref (+ b (* 8 (add1 j))))))))]
        [(str-bits? b)
         (if (zero? (untag b))
             (string)
             (build-string (heap-ref b)
                           (lambda (j)
                             (char-ref (+ b 8) j))))]
        [else (error "invalid bits")]))

(define (value->bits v)
  (cond [(eq? v #t) #b00011000]
        [(eq? v #f) #b00111000]
        [(integer? v) (arithmetic-shift v int-shift)]
        [(eof-object? v) #b01011000]
        [(void? v) #b01111000]
        [(empty? v)      #b10011000]
        [(char? v)
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]
        [else (error "not an immediate value" v)]))

(define (int-bits? v)
  (= type-int (bitwise-and v mask-int)))

(define (char-bits? v)
  (= type-char (bitwise-and v mask-char)))

(define (imm-bits? v)
  (zero? (bitwise-and v imm-mask)))

(define (cons-bits? v)
  (= type-cons (bitwise-and v imm-mask)))

(define (box-bits? v)
  (= type-box (bitwise-and v imm-mask)))

(define (vect-bits? v)
  (= type-vect (bitwise-and v imm-mask)))

(define (str-bits? v)
  (= type-str (bitwise-and v imm-mask)))

(define (untag i)
  (arithmetic-shift (arithmetic-shift i (- (integer-length ptr-mask)))
                    (integer-length ptr-mask)))

(define (heap-ref i)
  (ptr-ref (cast (untag i) _int64 _pointer) _int64))

(define (char-ref i j)
  (integer->char (ptr-ref (cast (untag i) _int64 _pointer) _uint32 j)))

