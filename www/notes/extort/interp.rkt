#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean


;; Expr -> Answer
(define (interp e)
  (match e
    [(int-e i) i]
    [(bool-e b) b]
    [(add1-e e0)
     (match (interp e0)
       [(? integer? i) (add1 i)]
       [_ 'err])]
    [(sub1-e e0)
     (match (interp e0)
       [(? integer? i) (sub1 i)]
       [_ 'err])]
    [(zero?-e e0)
     (match (interp e0)
       [(? integer? i) (zero? i)]
       [_ 'err])]
    [(if-e p e1 e2)
     (match (interp p)
       ['err 'err]
       [v
        (if v
            (interp e1)
            (interp e2))])]))
