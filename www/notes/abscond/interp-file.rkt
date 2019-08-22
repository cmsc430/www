#lang racket
(provide (all-defined-out))
(require "interp.rkt")

;; String -> Void
;; Parse and interpret contents of given filename,
;; print result on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((c (read-line)) ; ignore #lang racket line
            (p (read)))
        (unless (integer? p) (error "syntax error" p))
        (writeln (interp p))))))
