#lang racket
(provide interp)
(require "ast.rkt")

;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Value
(define (interp e)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Add1 e0)
     (add1 (interp e0))]
    [(Sub1 e0)
     (sub1 (interp e0))]
    [(Zero? e)
     (zero? (interp e))]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]))
