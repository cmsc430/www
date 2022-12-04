#lang racket
(provide a)
(require "b.rkt")

(define (a x)
  (+ (b x) (b x)))

(a 10)
