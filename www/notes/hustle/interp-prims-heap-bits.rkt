#lang racket
(provide interp-prim1 interp-prim2)
(require "types.rkt"
         "heap-bits.rkt")

;; Op1 Value* Heap -> Answer*
(define (interp-prim1 p v h)
  (match (list p v)
    [(list 'add1  (? int-bits? i))        (cons h (+ i (imm->bits 1)))]
    [(list 'sub1  (? int-bits? i))        (cons h (- i (imm->bits 1)))]
    [(list 'zero? (? int-bits? i))        (cons h (imm->bits (zero? i)))]
    [(list 'char? v)                      (cons h (imm->bits (char-bits? v)))]
    [(list 'char->integer (? char-bits?)) (cons h (imm->bits (char->integer (bits->imm v))))]
    [(list 'integer->char (? cp-bits?))   (cons h (imm->bits (integer->char (bits->imm v))))]
    [(list 'eof-object? v)                (cons h (= v (imm->bits eof)))]
    [(list 'write-byte (? byte-bits?))    (cons h (write-byte (bits->imm v)))]
    [(list 'box v)                        (alloc-box v h)]
    [(list 'unbox (? box-bits?  i))       (cons h (heap-ref h i))]
    [(list 'car   (? cons-bits? i))       (cons h (heap-ref h i))]
    [(list 'cdr   (? cons-bits? i))       (cons h (heap-ref h (+ i (arithmetic-shift 1 imm-shift))))]
    [(list 'empty? v)                     (cons h (= (imm->bits '()) v))]
    [_                                    'err]))

;; Op2 Value* Value* Heap -> Answer*
(define (interp-prim2 p v1 v2 h)
  (match (list p v1 v2)
    [(list '+ (? int-bits? i1) (? int-bits? i2)) (cons h (+ i1 i2))]
    [(list '- (? int-bits? i1) (? int-bits? i2)) (cons h (- i1 i2))]
    [(list 'cons v1 v2)                          (alloc-cons v1 v2 h)]
    [_                                           'err]))

;; Bits -> Boolean
(define (byte-bits? i)
  (and (int-bits? i)
       (<= (imm->bits 0) i (imm->bits 255))))

;; Bits -> Boolean
(define (cp-bits? v)
  (and (int-bits? v)
       (or (<= (imm->bits 0) v (imm->bits 55295))
           (<= (imm->bits 57344) v (imm->bits 1114111)))))