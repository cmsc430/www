#lang racket
(begin
    (define (map-add1 xs)
        (if (empty? xs)
            '()
            (cons (add1 (car xs))
                  (map-add1 (cdr xs)))))
    (map-add1 (cons 1 (cons 2 (cons 3 '())))))

