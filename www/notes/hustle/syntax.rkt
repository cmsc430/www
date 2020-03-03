#lang racket
(provide (all-defined-out))

;; Any -> Boolean
(define (binop? x)
  (and (symbol? x)
       (memq x '(+ - cons))))

;; Any -> Boolean
(define (unop? x)
  (and (symbol? x)
       (memq x '(add1 sub1 zero? box unbox car cdr))))

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? symbol?) (not (memq x '(if let)))]
    [(? integer?) #t]
    [(? boolean?) #t]
    [''() #t]
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
