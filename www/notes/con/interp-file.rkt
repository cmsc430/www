#lang racket
(provide main)
(require "parse.rkt" "interp.rkt")

;; String -> Void
;; Parse and interpret contents of given filename,
;; print result on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((_ (read-line))) ; ignore #lang racket line           
        (displayln (interp (parse (read))))))))
