#lang racket
(provide (all-defined-out))

;; type Expr =
;; | Integer
;; | Boolean
;; | Variable
;; | Prim1 Expr
;; | Prim2 Expr Expr
;; | If Expr Expr Expr
;; | Let [Binding] Expr

;; type Variable = Symbol (except 'add1 'sub1 'if 'let 'zero?)

;; type Binding = Variable Expr

(struct var-e (v) #:transparent)
(struct int-e (i) #:transparent)
(struct bool-e (b) #:transparent)

; the first argument to `prim-e` is a symbol the represents the
; primitive in question
(struct prim-e (prim e) #:transparent)
(struct if-e (p t f) #:transparent)
(struct let-e (bnd b) #:transparent)

(struct binding (var e) #:transparent)

;; A new AST node that denotes 'where' a variable is in our environment. This
;; is not important except for translate.rkt which is only for demonstration
(struct address-e (n) #:transparent)

(define prims '(add1 sub1 zero?))

(define (prim? p)
  ; We have to do thise to 'get rid' of the list value that `memq` returns
  (if (memq p prims) #t #f))

(define (value? v)
  (or (var-e? v)
      (int-e? v)
      (bool-e? v)))

(define (ast->sexpr a)
  (match a
    [(var-e v)    `(var-e ,v)]
    [(int-e i)    `(int-e ,i)]
    [(bool-e b)   `(bool-e ,b)]
    [(prim-e p e) `(prim-e ,p ,(ast->sexpr e))]
    [(if-e p t f) `(if-e ,(ast->sexpr p)
                         ,(ast->sexpr t)
                         ,(ast->sexpr f))]
    [(address-e i) `(address-e ,i)]
    [(let-e bd b) `(let-e ,(bindings->sexpr bd) ,(ast->sexpr b))]))

(define (bindings->sexpr bnd)
  (match bnd
    ['() '()]
    [bs  (map binding->sexpr bs)]))

(define (binding->sexpr bnd)
  (match bnd
    [(binding v e) `((,v ,(ast->sexpr e)))]))
