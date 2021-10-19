#lang racket
(provide translate)
(require "ast.rkt")

;; type IExpr =
;; | (Eof)
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Char Character)
;; | (Prim0 Op0)
;; | (Prim1 Op1 IExpr)
;; | (Prim2 Op2 IExpr IExpr)
;; | (If IExpr IExpr IExpr)
;; | (Begin IExpr IExpr)
;; | (Let '_ IExpr IExpr)
;; | (Var Addr)
;; type Addr = Natural

;; type LEnv = (Listof Id)

;; Expr -> IExpr
(define (translate e)
  (translate-e e '()))

;; Expr LEnv -> IExpr
(define (translate-e e r)
  (match e
    [(Eof)    e]
    [(Int i)  e]
    [(Bool b) e]   
    [(Char c) e]
    [(Prim0 p) e]
    [(Prim1 p e0)
     (Prim1 p (translate-e e0 r))]
    [(Prim2 p e0 e1)
     (Prim2 p
            (translate-e e0 r)
            (translate-e e1 r))]
    [(If e0 e1 e2)
     (If (translate-e e0 r)
         (translate-e e1 r)
         (translate-e e2 r))]
    [(Begin e0 e1)
     (Begin (translate-e e0 r)
            (translate-e e1 r))]               
    [(Let x e0 e1)
     (Let '_
          (translate-e e0 r)
          (translate-e e1 (cons x r)))]
    [(Var x)
     (Var (lexical-address x r))]))

;; Id LEnv -> Addr
(define (lexical-address x r)
  (match r
    [(cons y r)
     (match (symbol=? x y)
       [#t 0]
       [#f (add1 (lexical-address x r))])]))
