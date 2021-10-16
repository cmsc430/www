#lang racket

(define (read-int-list)
  (let ([x (read-byte)])
    (if (eof-object? x)
        '()
        (cons x (read-int-list)))))

(read-int-list)

