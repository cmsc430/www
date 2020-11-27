#lang racket
(provide alloc-box alloc-cons heap-ref)

;; Value* Heap -> Answer
(define (alloc-box v h)
  (cons (cons v h)
        (list 'box (length h))))

;; Value* Value* Heap -> Answer
(define (alloc-cons v1 v2 h)
  (cons (cons v1 (cons v2 h))
        (list 'cons (length h))))

;; Heap Address -> Value*
(define (heap-ref h a)
  (list-ref h a))
