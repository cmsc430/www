#lang racket
(provide (all-defined-out))
(require "ast.rkt" "translate.rkt" "interp-prim.rkt")

;; type VEnv = (Listof Value)

;; Expr -> Answer
(define (interp e)
  (interp-env (translate e) '()))

;; Expr VEnv -> Answer
(define (interp-env e r)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof) eof]       
    [(Var a) (list-ref r a)]
    [(Prim0 p) (interp-prim0 p)]     
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]
    [(Let '_ e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v (interp-env e2 (cons v r))])]))
