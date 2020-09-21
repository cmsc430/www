#lang racket
(provide (all-defined-out))
(require "ast.rkt")

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? integer? i) #t]
    [`(add1 ,x) (expr? x)]
    [`(sub1 ,x) (expr? x)]
    [_ #f]))

; SExpr -> AST
; convert the s-expr into our AST
; This should be a one-to-one mapping for now.
(define (sexpr->ast s)
  (match s
    [(? integer? s) (int-e s)]
    [`(add1 ,e)     (add1-e (sexpr->ast e))]
    [`(sub1 ,e)     (sub1-e (sexpr->ast e))]
    ['get-int       (get-i)]
    [_              (error "operation not supported")]))
