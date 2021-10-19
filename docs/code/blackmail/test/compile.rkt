#lang racket
(require "../compile.rkt" a86/interp "../parse.rkt" rackunit)

(define (run e)
  (asm-interp (compile (parse e))))

;; Abscond examples
(check-equal? (run 7) 7)
(check-equal? (run -8) -8)

;; Blackmail examples
(check-equal? (run '(add1 (add1 7))) 9)
(check-equal? (run '(add1 (sub1 7))) 7)
