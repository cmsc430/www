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
    [(? imm-bits?) (bits->imm v)]
    [(? box-bits? i)
     (box  (unload-value (heap-ref h i) h))]
    [(? cons-bits? i)
     (cons (unload-value (heap-ref h i) h)
           (unload-value (heap-ref h (+ i (arithmetic-shift 1 imm-shift))) h))]
    [(? str-bits? a)
     (let ((i (bits->imm (heap-ref h a)))
           (a (arithmetic-shift a (- imm-shift))))
       (list->string (map bits->imm (reverse (take (take-right h (+ a (add1 i))) i)))))]))