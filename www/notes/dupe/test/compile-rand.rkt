#lang racket
(require "../correct.rkt" "../random.rkt" rackunit)

(for ([i (in-range 500)])
  (check-correctness (random-expr)))
