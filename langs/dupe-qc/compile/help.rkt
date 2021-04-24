#lang racket
(provide (all-defined-out))

;; -> [List Label Label]
;; Guaranteed to be unique on each call
(define gen-if-labels
  (let ((i 0))
    (λ ()
      (set! i (add1 i))
      (list (lab "f" i)
            (lab "x" i)))))

;; String Integer -> Symbol
(define (lab s i)
  (string->symbol (string-append "if_" s "_" (number->string i))))
