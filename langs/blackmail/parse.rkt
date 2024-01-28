#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? exact-integer?) (Lit s)]
    [(list (? op1? o) e) (Prim1 o (parse e))]
    [_ (error "Parse error")]))

(define (op1? x)
  (memq x '(add1 sub1)))

