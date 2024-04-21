#lang racket
(provide read-all)
;; read all s-expression until eof
(define (read-all)
  (let ((r (read)))
    (if (eof-object? r)
        '()
        (cons r (read-all)))))

