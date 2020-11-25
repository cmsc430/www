#lang racket
(require "../correctness.rkt"
         "../random.rkt")

(for ([i (in-range 100)])
  (check-compiler (random-expr)))
