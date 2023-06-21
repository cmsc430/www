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
        [(proc-bits? b)
         (lambda _
           (error "This function is not callable."))]
	[else (error "bad bits!" b)]))

(define (value->bits v)
  (cond [(eof-object? v) val-eof]
        [(integer? v) (arithmetic-shift v int-shift)]
        [(char? v)
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]
        [(eq? v #t) val-true]
        [(eq? v #f) val-false]
        [(void? v)  val-void]
        [(empty? v) val-empty]
        [else (error "not an immediate value")]))

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

(define (untag i)
  (arithmetic-shift (arithmetic-shift i (- (integer-length ptr-mask)))
                    (integer-length ptr-mask)))

(define (heap-ref i)
  (ptr-ref (cast (untag i) _int64 _pointer) _int64))

(define (char-ref i j)
  (integer->char (ptr-ref (cast (untag i) _int64 _pointer) _uint32 j)))
