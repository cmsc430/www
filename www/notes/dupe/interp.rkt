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
    [(add1-e e0)
     (+ (interp e0) 1)]
    [(sub1-e e0)
     (- (interp e0) 1)]
    [(bool-e b) b]
    [(zero?-e e0)
     (zero? (interp e0))]
    [(if-e e0 e1 e2)
     (if (interp e0)
         (interp e1)
         (interp e2))]))
