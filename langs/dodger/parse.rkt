#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (cond
    [(integer? s) (Int s)]
    [(boolean? s) (Bool s)]
    [(char? s)    (Char s)]
    [else
     (match s
       [(list 'add1 e)  (Prim 'add1 (parse e))]
       [(list 'sub1 e)  (Prim 'sub1 (parse e))]
       [(list 'zero? e) (Prim 'zero? (parse e))]
       [(list 'char? e) (Prim 'char? (parse e))]
       [(list 'integer->char e) (Prim 'integer->char (parse e))]
       [(list 'char->integer e) (Prim 'char->integer (parse e))]
       [(list 'if e1 e2 e3)
        (If (parse e1) (parse e2) (parse e3))]
       [_ (error "Parse error")])]))
