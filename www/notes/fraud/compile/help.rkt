#lang racket
(provide (all-defined-out))

(define assert-integer
  `((mov rbx rax)
    (and rbx 1)
    (cmp rbx 0)
    (jne err)))
