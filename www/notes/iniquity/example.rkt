#lang racket
(begin
  (define (f x y) (+ x y))
  (let ((x (cons (f 1 2) '()))) x))
