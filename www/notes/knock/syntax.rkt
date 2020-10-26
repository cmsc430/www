#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? symbol?) (not (memq x '(if let)))]
    [(? integer?) #t]
    [(? boolean?) #t]
    [(? char?) #t]
    [''() #t]
    [`(,(? unop? p) ,x) (expr? x)]
    [`(,(? biop? p) ,x ,y)
      (and
        (expr? x)
        (expr? y))]
    [`(if ,x ,y ,z)
     (and (expr? x)
          (expr? y)
          (expr? z))]    
    [`(let ((,x ,y)) ,z)
     (and (symbol? x)
          (expr? y)
          (expr? z))]
    [`(,f . ,as)
      (and (symbol? f)
           (foldr (lambda (x y) (and x y)) #t (map expr? as)))]
    [`(define (,f . ,as) ,e1)
      (and (symbol? f)
           (expr? e1)
           (foldr (lambda (x y) (and x y)) #t (map symbol? as)))]
    [(list 'begin es ...)
      (foldr (lambda (x y) (and x y)) #t (map expr? es))]

    [_ #f]))

; SExpr -> Prog
(define (sexpr->prog s)
  (match s
    [(list 'begin defs ... e) (prog (map sexpr->fundef defs) (sexpr->expr e))]
    [e                        (prog '() (sexpr->expr e))]))

; SExpr -> FunDef
(define (sexpr->fundef def)
  (match def
    [`(define (,f . ,as) ,body) (fundef f as (sexpr->expr body))]))

; SExpr -> Expr
; Parse the s-expr into our Expr AST
; This should be a one-to-one mapping for now.
(define (sexpr->expr s)
  (match s
    [(? symbol? v)   (var-e v)]
    [(? integer? s)  (int-e s)]
    [(? boolean? b)  (bool-e b)]
    [(? char? c)     (char-e c)]
    [''()            (nil-e)]
    [`(fun ,f)       (fun-e f)]
    [`(call ,f ,@as) (call-e (sexpr->expr f) (map sexpr->expr as))]
    [`(if ,p ,t ,f) (if-e (sexpr->expr p) (sexpr->expr t) (sexpr->expr f))]
    [`(let ((,bnd ,def)) ,body) (let-e (list (binding bnd (sexpr->expr def))) (sexpr->expr body))]
    [`(,(? unop? p) ,e)
      (prim-e p (list (sexpr->expr e)))]
    [`(,(? biop? p) ,e1 ,e2)
      (prim-e p (list (sexpr->expr e1) (sexpr->expr e2)))]
    [`(,f . ,as)
      (app-e f (map sexpr->expr as))]
    [_              (error "operation not supported")]))
