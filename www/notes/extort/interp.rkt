#lang racket
(provide interp)
(require "ast.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Answer
(define (interp e)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Add1 e0)
     (match (interp e0)
       [(? integer? i) (add1 i)]
       [_ 'err])]
    [(Sub1 e0)
     (match (interp e0)
       [(? integer? i) (sub1 i)]
       [_ 'err])]
    [(Zero? e0)
     (match (interp e0)
       [(? integer? i) (zero? i)]
       [_ 'err])]
    [(If p e1 e2)
     (match (interp p)
       ['err 'err]
       [v
        (if v
            (interp e1)
            (interp e2))])]))
