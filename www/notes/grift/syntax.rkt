#lang racket
(provide (all-defined-out))

;; Any -> Boolean
(define (binop? x)
  (and (symbol? x)
       (memq x '(+ -))))

;; Any -> Boolean
(define (unop? x)
  (and (symbol? x)
       (memq x '(add1 sub1 zero?))))

;; Any -> Boolean
(define (value? x)
  (or (integer? x)
      (boolean? x)))

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? symbol?) (not (memq x '(if let add1 sub1 zero?)))]
    [(? integer?) #t]
    [(? boolean?) #t]
    [`(,(? unop? p) ,x) (expr? x)]
    [`(,(? binop? p) ,x ,y) 
      (and
        (expr? x)
        (expr? y))]
    [`(if ,x ,y ,z)
     (and (expr? x)
          (expr? y)
          (expr? z))]    
    [`(let ((,x ,y)) ,z)
     (and (symbol? x)
          (expr? y)
          (expr? z))]
    [_ #f]))
