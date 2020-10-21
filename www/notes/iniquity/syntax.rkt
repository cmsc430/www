#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; Any -> Boolean
(define (binop? x)
  (and (symbol? x)
       (memq x '(+ - cons))))

;; Any -> Boolean
(define (unop? x)
  (and (symbol? x)
       (memq x '(add1 sub1 zero? box unbox car cdr map-zero?))))

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? symbol?) (not (memq x '(if let)))]
    [(? integer?) #t]
    [(? boolean?) #t]
    [''() #t]
    [`(,(? unop? p) ,x) (expr? x)]
    [`(,(? binop? p) ,x ,y) 
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
    [`(define (,f . ,as) ,body) (fundef f as body)]))

; SExpr -> Expr
; Parse the s-expr into our Expr AST
; This should be a one-to-one mapping for now.
(define (sexpr->expr s)
  (match s
    [(? symbol? v)  (var-e v)]
    [(? integer? s) (int-e s)]
    [(? boolean? b) (bool-e b)]
    [''()           (nil-e)]
    [`(if ,p ,t ,f) (if-e (sexpr->expr p) (sexpr->expr t) (sexpr->expr f))]
    [`(let ((,bnd ,def)) ,body) (let-e (list (binding bnd (sexpr->expr def))) (sexpr->expr body))]
    [`(,p ,e)       (if (unop? p)
                        (prim-e p (list (sexpr->expr e)))
                        (error (format "~a is not a primitive" p)))]
    [`(,p ,e1 ,e2)  (if (biop? p)
                        (prim-e p (list (sexpr->expr e1) (sexpr->expr e2)))
                        (error (format "~a is not a primitive" p)))]
    [_              (error "operation not supported")]))
