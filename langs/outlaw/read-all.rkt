#lang racket
(provide read-all)
(require "read.rkt")
;; -> [Listof S-Expr]
;; read all s-expression until eof
(define (read-all)
  (let ((r (read)))
    (if (eof-object? r)
        '()
        (cons r (read-all)))))
