#lang racket
(begin
    (define (even? x)
        (if (zero? x)
            #t
            (odd? (sub1 x))))
    (define (odd? x)
        (if (zero? x)
            #f
            (even? (sub1 x))))
    (even? 42))

