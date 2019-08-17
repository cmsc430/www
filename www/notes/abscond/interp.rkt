#lang racket
(provide (all-defined-out))

; Expr -> Integer
; Interpret given expression
(define (abscond-interp e)
  e)

;; String -> Void
;; Parse and interpret contents of given filename,
;; print result on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((p (read)))
        (unless (integer? p) (error "syntax error" p))
        (writeln (abscond-interp p))))))
