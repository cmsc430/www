#lang racket
(provide alloc-box alloc-cons alloc-str heap-ref heap-set)

;; Value* Heap -> Answer
(define (alloc-box v h)
  (cons (cons v h)
        (list 'box (length h))))

;; Value* Value* Heap -> Answer
(define (alloc-cons v1 v2 h)
  (cons (cons v2 (cons v1 h))
        (list 'cons (length h))))

;; String Heap -> Answer
(define (alloc-str s h)
  (cons (append (reverse (string->list s)) (list (string-length s)) h)
        (list 'str (length h))))

;; Heap Address -> Value*
(define (heap-ref h a)
  (list-ref h (- (length h) (add1 a))))

;; Heap Address Value* -> Heap
(define (heap-set h i v)
  (list-set h (- (length h) i 1) v))
