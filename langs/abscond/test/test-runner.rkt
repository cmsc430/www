#lang racket
(provide test)
(require rackunit)

(define (test run)
  (begin ;; Abscond
    (check-equal? (run 7) 7)
    (check-equal? (run -8) -8)))


