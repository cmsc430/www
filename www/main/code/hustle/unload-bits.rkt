#lang racket
(provide unload unload-value)
(require "types.rkt"
         "heap-bits.rkt")

;; Answer* -> Answer
(define (unload a)
  (match a
    ['err 'err]
    [(cons h v) (unload-value v h)]))

;; Value* Heap -> Value
(define (unload-value v h)
  (match v
    [(? imm-bits?) (bits->value v)]
    [(? box-bits? i)
     (box  (unload-value (heap-ref h i) h))]
    [(? cons-bits? i)
     (cons (unload-value (heap-ref h i) h)
           (unload-value (heap-ref h (+ i (arithmetic-shift 1 imm-shift))) h))]))
