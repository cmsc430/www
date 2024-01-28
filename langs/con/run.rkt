#lang racket
(require a86/interp)
(provide run)

;; Asm -> Integer
(define (run is)
  (asm-interp is))

