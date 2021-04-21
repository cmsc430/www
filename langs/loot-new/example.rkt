#lang racket
(begin
  (define (id x) x)
  (define (close y) (let ((x 42))
		         (lambda (y) x)))
  (let ((f (lambda (x) (add1 x)))) ((close 0) 7)))
