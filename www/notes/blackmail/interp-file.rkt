#lang racket
(provide (all-defined-out))
(require "interp.rkt" "syntax.rkt")

;; String -> Void
;; Parse and interpret contents of given filename,
;; print result on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((p (read)))
        (unless (expr? p) (error "syntax error" p))
        (writeln (blackmail-interp p))))))
