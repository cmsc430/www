#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer? i) (Int i)]
    [(? boolean? b) (Bool b)]
    [(? symbol? x)  (Var x)]
    [(list 'add1 e) (Prim 'add1 (parse e))]
    [(list 'sub1 e) (Prim 'sub1 (parse e))]
    [(list 'zero? e) (Prim 'zero? (parse e))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse e1) (parse e2))]         
    [_ (error "Parse error")]))
