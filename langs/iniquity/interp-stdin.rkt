#lang racket
(provide main)
(require "parse.rkt" "interp.rkt" "read-all.rkt")

;; -> Void
;; Parse and interpret contents of stdin,
;; print result on stdout
(define (main)
  (read-line) ; ignore #lang racket line
  (let ((r (interp (parse (read-all)))))
    (unless (void? r)
      (println r))))
