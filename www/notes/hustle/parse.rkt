#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?)            (Int s)]
    [(? boolean?)            (Bool s)]
    [(? char?)               (Char s)]       
    ['eof                    (Eof)]
    [(? symbol?)             (Var s)]
    [(list 'quote (list))    (Empty)]
    [(list 'read-byte)       (Prim0 'read-byte)]       
    [(list 'add1 e)          (Prim1 'add1 (parse e))]
    [(list 'sub1 e)          (Prim1 'sub1 (parse e))]
    [(list 'zero? e)         (Prim1 'zero? (parse e))]
    [(list 'char? e)         (Prim1 'char? (parse e))]
    [(list 'write-byte e)    (Prim1 'write-byte (parse e))]
    [(list 'eof-object? e)   (Prim1 'eof-object? (parse e))]
    [(list 'integer->char e) (Prim1 'integer->char (parse e))]
    [(list 'char->integer e) (Prim1 'char->integer (parse e))]
    [(list 'box e)           (Prim1 'box (parse e))]
    [(list 'unbox e)         (Prim1 'unbox (parse e))]
    [(list 'empty? e)        (Prim1 'empty? (parse e))]
    [(list 'car e)           (Prim1 'car (parse e))]
    [(list 'cdr e)           (Prim1 'cdr (parse e))]
    [(list '+ e1 e2)         (Prim2 '+ (parse e1) (parse e2))]
    [(list '- e1 e2)         (Prim2 '- (parse e1) (parse e2))]    
    [(list 'cons e1 e2)      (Prim2 'cons (parse e1) (parse e2))]
    [(list 'begin e1 e2)     (Begin (parse e1) (parse e2))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse e1) (parse e2))]
    [_ (error "Parse error")]))

