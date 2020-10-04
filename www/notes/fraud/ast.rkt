#lang racket
(provide (all-defined-out))

;; type Expr =
;; | Integer
;; | Boolean
;; | Variable
;; | `(add1 ,Expr)
;; | `(sub1 ,Expr)
;; | `(zero? ,Expr)
;; | `(if ,Expr ,Expr ,Expr)
;; | `(let ((,Variable ,Expr)) ,Expr)

;; type Variable = Symbol (except 'add1 'sub1 'if 'let 'zero?)

(struct var-e (v))
(struct int-e (i))
(struct bool-e (b))

; the first argument to `prim-e` is a symbol the represents the
; primitive in question
(struct prim-e (prim e))
(struct if-e (p t f))
(struct let-e (bnd b))

(struct binding (var e))

(define prims '(add1 sub1 zero?))

(define (ast->sexpr a)
  (match a
    [(var-e v)    `(var-e ,v)]
    [(int-e i)    `(int-e ,i)]
    [(bool-e b)   `(bool-e ,b)]
    [(prim-e p e) `(prim-e ,p ,(ast->sexpr e))]
    [(if-e p t f) `(if-e ,(ast->sexpr p)
                         ,(ast->sexpr t)
                         ,(ast->sexpr f))]
    [(let-e bd b) `(let-e ,(bindings->sexpr bd) ,(ast->sexpr b))]))

(define (bindings->sexpr bnd)
  (match bnd
    ['() '()]
    [bs  (map binding->sexpr bs)]))

(define (binding->sexpr bnd)
  (match bnd
    [(binding v e) `((,(ast->sexpr v) ,(ast->sexpr e)))]))
