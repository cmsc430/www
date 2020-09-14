#lang racket
(provide (all-defined-out))
(require "interp.rkt")
(require "parse.rkt")

;; String -> Void
;; Parse and interpret contents of given filename,
;; print result on stdout
(define (main fn)
  (begin
  (define prog (with-input-from-file fn
    (λ ()
      (writeln "Interpreting..." (current-output-port))
      (let ((c (read-line)) ; ignore #lang racket line
            (p (read)))
        (parse p)))))
  (interp prog)))
