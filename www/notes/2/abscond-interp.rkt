#lang racket

; Expr -> Integer
; Interpreter for Abscond
(define (abscond-interp e)
  e)

(with-input-from-file (vector-ref (current-command-line-arguments) 0)
  (λ ()
    (let ((p (read)))
      (unless (integer? p)
        (error "syntax error" p))
      (writeln (abscond-interp p)))))


