#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Int s)]
    [(list 'add1 e) (Prim1 'add1 (parse e))]
    [(list 'sub1 e) (Prim1 'sub1 (parse e))]
    [(list 'if (list 'zero? e1) e2 e3)
     (IfZero (parse e1) (parse e2) (parse e3))]
    [_ (error "Parse error")]))
