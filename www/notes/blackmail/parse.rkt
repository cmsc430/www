#lang racket
(provide (all-defined-out))

(require "ast.rkt")

; SExpr -> AST
; Parse the s-expr into our AST
; This should be a one-to-one mapping for now.
(define (parse s)
  (match s
    [(? integer? s) (int-e s)]
    [`(add1 ,e)     (add1-e (parse e))]
    [`(sub1 ,e)     (sub1-e (parse e))]
    ['get-int       (get-i)]
    [_              (error "operation not supported")]))
