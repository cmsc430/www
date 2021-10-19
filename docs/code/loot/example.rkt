#lang racket
(begin (define (f x) (if (zero? x) 0 (f (sub1 x))))
       (f 1))
