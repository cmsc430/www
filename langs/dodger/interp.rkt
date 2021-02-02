#lang racket
(provide interp)
(require "ast.rkt" "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean
;; | Character

;; Expr -> Value
(define (interp e)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Prim1 p e)
     (interp-prim1 p (interp e))]    
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]))
