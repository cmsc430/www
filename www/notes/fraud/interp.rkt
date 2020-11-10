#lang racket
(provide interp interp-env interp-prim)
(require "ast.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean

;; type REnv = (Listof (List Id Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Var x) (lookup r x)]
    [(Int i) i]
    [(Bool b) b]
    [(Prim p e) (interp-prim p (interp-env e r))]
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Let x e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v (interp-env e2 (ext r x v))])]))

;; Op Answer -> Answer
(define (interp-prim p a)
  (match a
    [(? integer? i)
     ((match p
        ['add1 add1]
        ['sub1 sub1]
        ['zero? zero?])
      i)]
    [_ 'err]))

;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))

;; Env Id Value -> Env
(define (ext r v val)
  (cons (list v val) r))
