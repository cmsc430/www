#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer? i) (Int i)]
    [(list 'add1 e) (Add1 (parse e))]
    [(list 'sub1 e) (Sub1 (parse e))]
    [(list 'if z1 e2 e3)
     (match z1
       [(list 'zero? e1)
        (IfZero (parse e1) (parse e2) (parse e3))])]
    [_ (error "Parse error")]))
