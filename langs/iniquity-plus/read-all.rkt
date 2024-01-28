#lang racket
(provide read-all)
;; read all s-expression until eof
(define (read-all p)
  (let ((r (read p)))
    (if (eof-object? r)
        '()
        (cons r (read-all p)))))
