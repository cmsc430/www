#lang racket
(provide alloc-box alloc-cons heap-ref heap-set)
(require "types.rkt")

;; Value* Heap -> Answer
(define (alloc-box v h)
  (cons (cons v h)
        (bitwise-ior (arithmetic-shift (length h) imm-shift)
                     type-box)))

;; Value* Value* Heap -> Answer
(define (alloc-cons v1 v2 h)
  (cons (cons v2 (cons v1 h))
        (bitwise-ior (arithmetic-shift (length h) imm-shift)
                     type-cons)))

;; Heap Address -> Value*
(define (heap-ref h a)
  (let ((a (arithmetic-shift a (- imm-shift))))
    (list-ref h (- (length h) (add1 a)))))

;; Heap Address Value* -> Heap
(define (heap-set h a v)
  (let ((a (arithmetic-shift a (- imm-shift))))
    (list-set h (- (length h) a 1) v)))
