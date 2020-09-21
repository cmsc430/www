#lang racket
(provide (all-defined-out))
(require "ast.rkt")

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? integer?) #t]
    [(? boolean?) #t]
    [`(add1 ,x) (expr? x)]
    [`(sub1 ,x) (expr? x)]
    [`(zero? ,x) (expr? x)]
    [`(if (zero? ,x) ,y ,z)
     (and (expr? x)
          (expr? y)
          (expr? z))]
    [_ #f]))


; SExpr -> AST
; Parse the s-expr into our AST
; This should be a one-to-one mapping for now.
(define (sexpr->ast s)
  (match s
    [(? integer? s) (int-e s)]
    [`(add1 ,e)     (add1-e (sexpr->ast e))]
    [`(sub1 ,e)     (sub1-e (sexpr->ast e))]
    [`(if (zero? ,i) ,t ,f) (if-e (sexpr->ast i) (sexpr->ast t) (sexpr->ast f))]
    [_              (error "operation not supported")]))
