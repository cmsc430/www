#lang racket
;; type Expr =
;; | Integer
;; | Variable
;; | `(add1 ,Expr)
;; | `(sub1 ,Expr)
;; | `(,Prim2 ,Expr ,Expr)
;; | `(if (zero? ,Expr) ,Expr ,Expr)
;; | `(let ((,Variable ,Expr)) ,Expr)

;; type Prim2 =
;; | '*
;; | '+
;; | '-

;; type ANF =
;; | Imm
;; | `(add1 ,ANF)
;; | `(sub1 ,ANF)
;; | `(,Prim2 ,Imm ,Imm)
;; | `(if (zero? ,ANF) ,ANF ,ANF)
;; | `(let ((,Variable ,ANF)) ,ANF)

;; type Variable = Symbol
