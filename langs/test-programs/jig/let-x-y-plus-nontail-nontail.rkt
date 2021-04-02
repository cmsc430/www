#lang racket

(begin (define (double x) (+ x x)) (let ((x 2)) (let ((y 3)) (+ (double 2) (double 3)))))