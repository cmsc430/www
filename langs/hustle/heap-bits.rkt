#lang racket
(provide alloc-box alloc-cons heap-ref)
(require "types.rkt")

;; Value* Heap -> Answer
(define (alloc-box v h)
  (cons (cons v h)
        (bitwise-ior (arithmetic-shift (length h) imm-shift)
                     type-box)))

;; Value* Value* Heap -> Answer
(define (alloc-cons v1 v2 h)
  (cons (cons v1 (cons v2 h))
        (bitwise-ior (arithmetic-shift (length h) imm-shift)
                     type-cons)))

;; Heap Address -> Value*
(define (heap-ref h a)
  (list-ref h (arithmetic-shift a (- imm-shift))))