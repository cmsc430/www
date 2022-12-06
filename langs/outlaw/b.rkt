#lang racket
(provide b)
(require "c.rkt")

(define (b x)
  (add1 (c x)))
