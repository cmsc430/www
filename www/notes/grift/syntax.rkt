#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? symbol?) (not (memq x '(if let add1 sub1 zero?)))]
    [(? integer?) #t]
    [(? boolean?) #t]
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
    [_ #f]))

; SExpr -> AST
; Parse the s-expr into our AST
; This should be a one-to-one mapping for now.
(define (sexpr->ast s)
  (match s
    [(? symbol? v)  (var-e v)]
    [(? integer? s) (int-e s)]
    [(? boolean? b) (bool-e b)]
    [`(if ,p ,t ,f) (if-e (sexpr->ast p) (sexpr->ast t) (sexpr->ast f))]
    [`(let ((,bnd ,def)) ,body) (let-e (list (binding bnd (sexpr->ast def))) (sexpr->ast body))]
    [`(,p ,e)       (if (unop? p)
                        (prim-e p (list (sexpr->ast e)))
                        (error (format "~a is not a primitive" p)))]
    [`(,p ,e1, e2)  (if (biop? p)
                        (prim-e p (list (sexpr->ast e1) (sexpr->ast e2)))
                        (error (format "~a is not a primitive" p)))]
    [_              (error "operation not supported")]))
