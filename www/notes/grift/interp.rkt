#lang racket
(provide interp interp-env interp-prim1 interp-prim2)
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
    [(Prim1 p e)
     (interp-prim1 p (interp-env e r))]
    [(Prim2 p e1 e2)
     (interp-prim2 p (interp-env e1 r) (interp-env e2 r))]
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

;; Op1 Answer -> Answer
(define (interp-prim1 p a)
  (match (list p a)
    [(list 'add1 (? integer? i)) (add1 i)]
    [(list 'sub1 (? integer? i)) (sub1 i)]
    [(list 'zero? (? integer? i)) (zero? i)]
    [_ 'err]))

;; Op2 Answer Answer -> Answer
(define (interp-prim2 p a1 a2)
  (match (list p a1 a2)
    [(list '+ (? integer? i1) (? integer? i2)) (+ i1 i2)]
    [(list '- (? integer? i1) (? integer? i2)) (- i1 i2)]
    [_ 'err]))

;; Env Id -> Ans
(define (lookup r x)
  (match r
    ['() 'err]
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))

;; Env Id Value -> Env
(define (ext r v val)
  (cons (list v val) r))
