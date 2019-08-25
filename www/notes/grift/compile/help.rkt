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


(define true-rep  #b10011111)
(define false-rep #b00011111)
(define fixnum-mask #b11)
(define fixnum-shift 2)
(define bool-shift   8)

(define assert-integer
  `((mov rbx rax)
    (and rbx ,fixnum-mask)
    (cmp rbx 0)
    (jne err)))
