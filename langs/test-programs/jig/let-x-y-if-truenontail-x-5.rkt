#lang racket

(begin (define (double x) (+ x x)) (let ((x 2)) (let ((y 3)) (if (double 5) x 5))))