#lang racket
(provide interp-prim1 interp-prim2)
(require (except-in "types.rkt" heap-ref)
         "heap-bits.rkt")

;; Op1 Value* Heap -> Answer*
(define (interp-prim1 p v h)
  (match (list p v)
    [(list 'add1  (? int-bits? i))        (cons h (+ i (value->bits 1)))]
    [(list 'sub1  (? int-bits? i))        (cons h (- i (value->bits 1)))]
    [(list 'zero? (? int-bits? i))        (cons h (value->bits (zero? i)))]
    [(list 'char? v)                      (cons h (value->bits (char-bits? v)))]
    [(list 'char->integer (? char-bits?)) (cons h (value->bits (char->integer (bits->value v))))]
    [(list 'integer->char (? cp-bits?))   (cons h (value->bits (integer->char (bits->value v))))]
    [(list 'eof-object? v)                (cons h (value->bits (= v (value->bits eof))))]
    [(list 'write-byte (? byte-bits?))    (cons h (begin (write-byte (bits->value v)) (value->bits (void))))]
    [(list 'box v)                        (alloc-box v h)]
    [(list 'unbox (? box-bits?  i))       (cons h (heap-ref h i))]
    [(list 'car   (? cons-bits? i))       (cons h (heap-ref h i))]
    [(list 'cdr   (? cons-bits? i))       (cons h (heap-ref h (+ i (arithmetic-shift 1 imm-shift))))]
    [(list 'empty? v)                     (cons h (value->bits (= (value->bits '()) v)))]
    [_                                    'err]))

;; Op2 Value* Value* Heap -> Answer*
(define (interp-prim2 p v1 v2 h)
  (match (list p v1 v2)
    [(list '+ (? int-bits? i1) (? int-bits? i2)) (cons h (+ i1 i2))]
    [(list '- (? int-bits? i1) (? int-bits? i2)) (cons h (- i1 i2))]
    [(list '< (? int-bits? i1) (? int-bits? i2)) (cons h (value->bits (< i1 i2)))]
    [(list '= (? int-bits? i1) (? int-bits? i2)) (cons h (value->bits (= i1 i2)))]
    [(list 'eq? v1 v2)                           (cons h (value->bits (= v1 v2)))]
    [(list 'cons v1 v2)                          (alloc-cons v1 v2 h)]
    [_  'err]))

;; Bits -> Boolean
(define (byte-bits? i)
  (and (int-bits? i)
       (<= (value->bits 0) i (value->bits 255))))

;; Bits -> Boolean
(define (cp-bits? v)
  (and (int-bits? v)
       (or (<= (value->bits 0) v (value->bits 55295))
           (<= (value->bits 57344) v (value->bits 1114111)))))
