#lang racket
(provide interp)
(require "ast.rkt")

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
    [(Prim1 'add1 e0)
     (add1 (interp e0))]
    [(Prim1 'sub1 e0)
     (sub1 (interp e0))]
    [(Prim1 'zero? e)
     (zero? (interp e))]
    [(Prim1 'integer->char e)
     (integer->char (interp e))]
    [(Prim1 'char->integer e)
     (char->integer (interp e))]
    [(Prim1 'char? e)
     (char? (interp e))]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]))
