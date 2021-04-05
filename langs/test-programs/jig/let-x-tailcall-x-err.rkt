#lang racket

(begin (define (double x) (+ x x)) (let ((x #f)) (double x)))