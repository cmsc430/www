#lang racket
(provide (all-defined-out))

(require "ast.rkt")

; SExpr -> Expr
; Parse the s-expr into our AST
; This should be a one-to-one mapping for now.
(define (parse s)
  (match s
    [(? integer? s) (int-e s)]
    ['get-int       (get-i)]
    [_              (error "operation not supported")]))
