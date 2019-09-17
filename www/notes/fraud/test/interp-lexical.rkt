#lang racket
(require rackunit)
(require "../interp-lexical.rkt")
(check-equal? (interp 5) 5)
(check-equal? (interp '(let ((x 0)) x)) 0)
(check-equal? (interp '(let ((x 0)) (let ((y 1)) x))) 0)
(check-equal? (interp '(let ((x 0)) (let ((y 1)) y))) 1)
(check-equal? (interp '(let ((x 0)) (let ((y x)) y))) 0)
