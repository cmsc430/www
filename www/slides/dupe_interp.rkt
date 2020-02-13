#lang racket
(provide (all-defined-out))

;; The sorts of things that `dupe` programs can result in:
;;
;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Value
(define (interp e)
  (match e
    ;; Integers and booleans are straightforward
    [(? integer? i) i]
    [(? boolean? b) b]








    ;; add1 and sub1 are the same from blackmail
    [`(add1 ,e0)
     (add1 (interp e0))]
    [`(sub1 ,e0)
     (sub1 (interp e0))]







    ;; zero? is now it's own expression, and results in a boolean
    [`(zero? ,e0)
     (zero? (interp e0))]











    ;; The rule for if is straightforward, but what does
    ;; it imply about type mismatches?
    [`(if ,e0 ,e1 ,e2)
     (if (interp e0)
         (interp e1)
         (interp e2))]))







