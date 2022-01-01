#lang racket
(provide read-all)
(require "stdlib.rkt")
;; -> [Listof S-Expr]
;; read all s-expression until eof
(define (read-all)
  (let ((r (read)))
    (if (eof-object? r)
        '()
        (cons r (read-all)))))
