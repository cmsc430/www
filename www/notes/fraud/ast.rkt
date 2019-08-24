#lang racket
;; type Expr =
;; | Integer
;; | Boolean
;; | Variable
;; | `(add1 ,Expr)
;; | `(sub1 ,Expr)
;; | `(zero? ,Expr)
;; | `(if ,Expr ,Expr ,Expr)
;; | `(let ((,Variable ,Expr)) ,Expr)

;; type Variable = Symbol (except 'add1 'sub1 'if 'let)
