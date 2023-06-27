#lang crook
{:= A B C D0 D1 E0 E1 F H0 H1}
(provide main)
(require "parse.rkt")
(require "interp.rkt")

;; -> Void
;; Parse and interpret contents of stdin,
;; print result on stdout
(define (main)
  (read-line) ; ignore #lang racket line
  (println (interp (parse (read)))))
