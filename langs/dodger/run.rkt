#lang racket
(require a86/interp)
(require "types.rkt")
(provide run)
;; Asm -> Value
(define (run is)
  (bits->value (asm-interp is)))

