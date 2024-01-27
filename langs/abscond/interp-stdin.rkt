#lang racket
(provide main)
(require "parse.rkt")
(require "interp.rkt")

;; -> Void
;; Parse and interpret contents of stdin,
;; print result on stdout
(define (main)
  (read-line) ; ignore #lang racket line
  (println (interp (parse (read)))))

