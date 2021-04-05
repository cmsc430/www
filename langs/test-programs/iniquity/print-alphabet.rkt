#lang racket
(begin
    (define (print-alphabet i)
        (if (zero? i)
            (void)
            (begin (write-byte (- 123 i))
                   (print-alphabet (sub1 i)))))
    (print-alphabet 26))
