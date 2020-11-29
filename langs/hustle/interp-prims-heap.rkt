#lang racket
(provide interp-prim1 interp-prim2 interp-prim3)
(require "heap.rkt")

;; Op1 Value* Heap -> Answer*
(define (interp-prim1 p v h)
  (match (list p v)
    [(list 'add1 (? integer? i))          (cons h (add1 i))]
    [(list 'sub1 (? integer? i))          (cons h (sub1 i))]
    [(list 'zero? (? integer? i))         (cons h (zero? i))]
    [(list 'char? v)                      (cons h (char? v))]
    [(list 'char->integer (? char?))      (cons h (char->integer v))]
    [(list 'integer->char (? codepoint?)) (cons h (integer->char v))]
    [(list 'eof-object? v)                (cons h (eof-object? v))]
    [(list 'write-byte (? byte?))         (cons h (write-byte v))]
    [(list 'box v)                        (alloc-box v h)]
    [(list 'unbox (list 'box i))          (cons h (heap-ref h i))]
    [(list 'car   (list 'cons i))         (cons h (heap-ref h i))]
    [(list 'cdr   (list 'cons i))         (cons h (heap-ref h (add1 i)))]
    [(list 'empty? v)                     (cons h (empty? v))]
    [(list 'string? (list 'str s))        (cons h #t)]
    [(list 'string? v)                    (cons h #f)]
    [_                                    'err]))

;; Op2 Value* Value* Heap -> Answer*
(define (interp-prim2 p v1 v2 h)
  (match (list p v1 v2)
    [(list '+ (? integer? i1) (? integer? i2)) (cons h (+ i1 i2))]
    [(list '- (? integer? i1) (? integer? i2)) (cons h (- i1 i2))]
    [(list 'cons v1 v2)                        (alloc-cons v1 v2 h)]
    [(list 'make-string (? integer? i) (? char? c))
     (if (<= 0 i)
         (alloc-str (make-string i c) h)
         'err)]
    [(list 'string-ref (list 'str a) (? integer? i))
     (cons h (heap-ref h (+ a i 1)))]
    [_ 'err]))

;; Op2 Value* Value* Heap -> Answer*
(define (interp-prim3 p v1 v2 v3 h)
  (match (list p v1 v2 v3)
    [(list 'string-set! (list 'str a) (? integer? i) (? char? c))
     (if (<= 0 v2 (sub1 (heap-ref h a)))
         (cons (heap-set h (+ a i 1) c)
               (void))
         'err)]
    [_  'err]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
