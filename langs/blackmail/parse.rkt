#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (cond
    [(integer? s) (Int s)]
    [else
     (match s
       [(list 'add1 e) (Prim 'add1 (parse e))]
       [(list 'sub1 e) (Prim 'sub1 (parse e))]
       [_ (error "Parse error")])]))
