#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Prog
(define (parse s)
  (match s
    [(list 'begin ds ... e)
     (Prog (map parse-d ds) (parse-e e))]
    [e (Prog '() (parse-e e))]))

;; S-Expr -> Defn
(define (parse-d s)
  (match s
    [(list 'define (list (? symbol? f) (? symbol? xs) ...) e)
     (Defn f xs (parse-e e))]
    [_ (error "Parse error")]))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? integer? i)       (Int i)]
    [(? boolean? b)       (Bool b)]
    [(? symbol? x)        (Var x)]
    [(list 'quote (list)) (Empty)]
    [(list 'add1 e)       (Prim1 'add1 (parse-e e))]
    [(list 'sub1 e)       (Prim1 'sub1 (parse-e e))]
    [(list 'zero? e)      (Prim1 'zero? (parse-e e))]
    [(list 'box e)        (Prim1 'box (parse-e e))]
    [(list 'unbox e)      (Prim1 'unbox (parse-e e))]
    [(list 'empty? e)     (Prim1 'empty? (parse-e e))]
    [(list 'car e)        (Prim1 'car (parse-e e))]
    [(list 'cdr e)        (Prim1 'cdr (parse-e e))]
    [(list '+ e1 e2)      (Prim2 '+ (parse-e e1) (parse-e e2))]
    [(list '- e1 e2)      (Prim2 '- (parse-e e1) (parse-e e2))]
    [(list 'cons e1 e2)   (Prim2 'cons (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    [(cons (? symbol? f) es)
     (App f (map parse-e es))]
    [_ (error "Parse error")]))
