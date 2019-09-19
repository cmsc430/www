#lang racket
(provide (all-defined-out))

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? symbol?) (not (memq x '(if let add1 sub1)))]
    [(? integer? i) #t]
    [`(add1 ,x) (expr? x)]
    [`(sub1 ,x) (expr? x)]
    [`(if (zero? ,x) ,y ,z)
     (and (expr? x)
          (expr? y)
          (expr? z))]    
    [`(let ((,x ,y)) ,z)
     (and (symbol? x)
          (expr? y)
          (expr? z))]
    [_ #f]))
