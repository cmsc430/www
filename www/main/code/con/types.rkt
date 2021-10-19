#lang racket
(provide (all-defined-out))

;; type Value =
;; | Integer

;; type Bits = Integer

(define int-shift    0)

;; Bits -> Value
(define (bits->value b) b)

;; Value -> Bits
(define (value->bits v)
  (match v
    [(? integer?) (arithmetic-shift v int-shift)]))
