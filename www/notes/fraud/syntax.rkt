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
    [`(add1 ,x) (expr? x)]
    [`(sub1 ,x) (expr? x)]
    [`(zero? ,x) (expr? x)]
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
    [`(address ,e)  (address-e e)]
    [`(,p ,e)       (if (memq p prims)
                        (prim-e p (sexpr->ast e))
                        (error (format "~a is not a primitive" p)))]
    [`(if ,p ,t ,f) (if-e (sexpr->ast p) (sexpr->ast t) (sexpr->ast f))]
    [`(let ((,bnd ,def)) ,body) (let-e (list (binding bnd (sexpr->ast def))) (sexpr->ast body))]
    [_              (error "operation not supported")]))
