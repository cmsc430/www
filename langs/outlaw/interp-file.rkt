#lang racket
(provide main)
(require "parse.rkt" "interp.rkt" "read-all.rkt")

;; String -> Void
;; Parse and interpret contents of given filename,
;; print result on stdout
(define (main fn)
  (let ((p (open-input-file fn)))
    (begin
      (read-line p) ; ignore #lang racket line
      (let ((r (interp (parse (read-all p)))))
        (unless (void? r)
          (println r)))
      (close-input-port p))))
