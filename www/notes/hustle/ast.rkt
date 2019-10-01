#lang racket
;; type Expr =
;; | Integer
;; | Boolean
;; | Variable
;; | (list Prim1 Expr)
;; | (list Prim2 Expr Expr)
;; | `(if ,Expr ,Expr ,Expr)
;; | `(let ((,Variable ,Expr)) ,Expr)

;; type Prim1 =
;; | 'add1 | 'sub1 | 'zero?
;; | 'box | 'unbox | 'car | 'cdr
;; type Prim2 =
;; | '+ | '- | 'cons

;; type Variable = Symbol (except 'add1 'sub1 'if, etc.)
