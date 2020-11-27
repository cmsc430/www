#lang racket
(provide main)
(require "parse.rkt" "interp.rkt")

;; String -> Void
;; Parse and interpret contents of given filename,
;; print result on stdout
(define (main fn)
  (let ((p (open-input-file fn)))
    (begin
      (read-line p) ; ignore #lang racket line
      (displayln (interp (parse (read p))))
      (close-input-port p))))
