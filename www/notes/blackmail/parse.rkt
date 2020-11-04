#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer? i) (Int i)]
    [(list 'add1 e) (Add1 (parse e))]
    [(list 'sub1 e) (Sub1 (parse e))]
    [_ (error "Parse error")]))
