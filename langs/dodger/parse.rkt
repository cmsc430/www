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
       [(list 'add1 e)  (Prim1 'add1 (parse e))]
       [(list 'sub1 e)  (Prim1 'sub1 (parse e))]
       [(list 'zero? e) (Prim1 'zero? (parse e))]
       [(list 'char? e) (Prim1 'char? (parse e))]
       [(list 'integer->char e) (Prim1 'integer->char (parse e))]
       [(list 'char->integer e) (Prim1 'char->integer (parse e))]
       [(list 'if e1 e2 e3)
        (If (parse e1) (parse e2) (parse e3))]
       [_ (error "Parse error")])]))
