#lang racket
;; type Expr =
;; | Integer
;; | Variable
;; | `(add1 ,Expr)
;; | `(sub1 ,Expr)
;; | `(if (zero? ,Expr) ,Expr ,Expr)
;; | `(let ((,Variable ,Expr)) ,Expr)

;; type Variable = Symbol
