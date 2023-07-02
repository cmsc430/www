#lang crook
{:= D0 D1 E0 E1 F H0 H1 I J}
(provide (all-defined-out))
{:> H0} (require ffi/unsafe)

{:> H0} (define imm-shift          3)
{:> H0} (define imm-mask       #b111)
{:> H0} (define ptr-mask       #b111)
{:> H0} (define type-box       #b001)
{:> H0} (define type-cons      #b010)
{:> H1} (define type-vect      #b011)
{:> H1} (define type-str       #b100)
(define int-shift    {:> D0 H0} 1 {:> H0} (+ 1 imm-shift))
(define mask-int   {:> D0 H0} #b1 {:> H0} #b1111)
{:> D1}
(define char-shift   {:> D1 H0} 2 {:> H0} (+ 2 imm-shift))
(define type-int   {:> D0 H0} #b0 {:> H0} #b0000)
{:> D1}
(define type-char {:> D0 H0} #b01 {:> H0} #b01000)
{:> D1}
(define mask-char {:> D0 H0} #b11 {:> H0} #b11111)

(define (bits->value b)
  (cond [(= b (value->bits #t))  #t]
        [(= b (value->bits #f)) #f]
        {:> E0}
        [(= b (value->bits eof))  eof]
        {:> E0}
        [(= b (value->bits (void))) (void)]
        {:> H0}
        [(= b (value->bits '())) '()]
        [(int-bits? b)
         (arithmetic-shift b (- int-shift))]
        {:> D1}
        [(char-bits? b)
         (integer->char (arithmetic-shift b (- char-shift)))]
        {:> H0}
        [(box-bits? b)
         (box (bits->value (heap-ref b)))]
        {:> H0}
        [(cons-bits? b)
         (cons (bits->value (heap-ref (+ b 8)))
               (bits->value (heap-ref b)))]
        {:> H1}
        [(vect-bits? b)
         (if (zero? (untag b))
             (vector)
             (build-vector (heap-ref b)
                           (lambda (j)
                             (bits->value (heap-ref (+ b (* 8 (add1 j))))))))]
        {:> H1}
        [(str-bits? b)
         (if (zero? (untag b))
             (string)
             (build-string (heap-ref b)
                           (lambda (j)
                             (char-ref (+ b 8) j))))]
        [else (error "invalid bits")]))

(define (value->bits v)
  (cond [(eq? v #t) {:> D0 H0} #b011 {:> H0} #b00011000]
        [(eq? v #f) {:> D0 H0} #b111 {:> H0} #b00111000]
        [(integer? v) (arithmetic-shift v int-shift)]
        {:> E0} [(eof-object? v) {:> E0 H0} #b1011 {:> H0} #b01011000]
        {:> E0} [(void? v)       {:> E0 H0} #b1111 {:> H0} #b01111000]
        {:> H0} [(empty? v)      #b10011000]
        {:> D1}
        [(char? v)
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]
        {:> H0}
        [else (error "not an immediate value" v)]))

(define (int-bits? v)
  (= type-int (bitwise-and v mask-int)))

{:> D1}
(define (char-bits? v)
  (= type-char (bitwise-and v mask-char)))

{:> H0}
(define (imm-bits? v)
  (zero? (bitwise-and v imm-mask)))

{:> H0}
(define (cons-bits? v)
  (= type-cons (bitwise-and v imm-mask)))

{:> H0}
(define (box-bits? v)
  (= type-box (bitwise-and v imm-mask)))

{:> H1}
(define (vect-bits? v)
  (= type-vect (bitwise-and v imm-mask)))

{:> H1}
(define (str-bits? v)
  (= type-str (bitwise-and v imm-mask)))

{:> H0}
(define (untag i)
  (arithmetic-shift (arithmetic-shift i (- (integer-length ptr-mask)))
                    (integer-length ptr-mask)))

{:> H0}
(define (heap-ref i)
  (ptr-ref (cast (untag i) _int64 _pointer) _int64))

{:> H1}
(define (char-ref i j)
  (integer->char (ptr-ref (cast (untag i) _int64 _pointer) _uint32 j)))
