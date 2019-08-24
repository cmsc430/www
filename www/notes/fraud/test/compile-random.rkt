#lang racket
(require "../correctness.rkt"
         "../random.rkt")

(for ([i (in-range 500)])
  (check-compiler (random-expr)))
