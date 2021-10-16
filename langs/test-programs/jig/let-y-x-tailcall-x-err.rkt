#lang racket
(define (double x) (+ x x))
(let ((y 3)) (let ((x #f)) (double x)))
