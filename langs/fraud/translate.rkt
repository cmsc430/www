#lang racket
(provide translate)
(require "ast.rkt")

;; type IExpr =
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Prim Op Expr)
;; | (If Expr Expr Expr)
;; | (Let '_ Expr Expr)
;; | (Var Addr)
;; type Addr = Natural
;; type prim = 'add1 | 'sub1 | 'zero?

;; type LEnv = (Listof Id)

;; Expr -> IExpr
(define (translate e)
  (translate-e e '()))

;; Expr LEnv -> IExpr
(define (translate-e e r)
  (match e
    [(Var x)
     (Var (lexical-address x r))]
    [(Int i) e]
    [(Bool b) e]   
    [(Prim1 p e)
     (Prim1 p (translate-e e r))]
    [(If e0 e1 e2)
     (If (translate-e e0 r)
         (translate-e e1 r)
         (translate-e e2 r))]
    [(Let x e1 e2)
     (Let '_
          (translate-e e1 r)
          (translate-e e2 (cons x r)))]))

;; Id LEnv -> Addr
(define (lexical-address x r)
  (match r
    [(cons y r)
     (match (symbol=? x y)
       [#t 0]
       [#f (add1 (lexical-address x r))])]))
