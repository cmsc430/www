#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Value
(define (interp e)
  (match e
    [(int-e i) i]
    [(bool-e b) b]
    [(add1-e e0)
     (+ (interp e0) 1)]
    [(sub1-e e0)
     (- (interp e0) 1)]
    [(zero?-e e0)
     (zero? (interp e0))]
    [(if-e p e1 e2)
     (if (interp p)
         (interp e1)
         (interp e2))]))
