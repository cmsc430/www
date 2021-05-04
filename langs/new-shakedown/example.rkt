#lang racket
(begin
  (define (map f xs)
    (if (empty? xs)
        '()
        (cons (f (car xs)) (map f (cdr xs)))))
  (define (f x)
    (ccall c_fun_arg x))
  (map f (cons 2 (cons 4 (cons 8 '())))))

;(let ((f (lambda (x) (ccall c_fun_arg x))))
;     (f 8))
;((x (ccall c_fun)))
;      (let ((y (ccall c_fun_arg x))) y))
