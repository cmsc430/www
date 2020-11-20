#lang racket
(provide (all-defined-out))
(require "ast.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | '()
;; | (box Value)
;; | (cons Value Value)

;; type REnv = (Listof (List Id Value))
;; type Defns = (Listof Defn)

;; Prog -> Answer
(define (interp p)
  (match p
    [(Prog ds e)
     (interp-env e '() ds)]))

;; Expr REnv Defns -> Integer
(define (interp-env e r ds)
  (match e
    [(Var x) (lookup r x)]
    [(Int i) i]
    [(Bool b) b]
    [(Empty) '()]
    [(Prim1 p e)
     (interp-prim1 p (interp-env e r ds))]
    [(Prim2 p e1 e2)
     (interp-prim2 p (interp-env e1 r ds) (interp-env e2 r ds))]
    [(If p e1 e2)
     (match (interp-env p r ds)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r ds)
            (interp-env e2 r ds))])]
    [(Let x e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v (interp-env e2 (ext r x v) ds)])]
    
    [(App f es)
     (match (interp-env* es r ds)
       [(list vs ...)
        (match (defns-lookup ds f)
          [(Defn f xs e)
           ; check arity matches
           (if (= (length xs) (length vs))
               (interp-env e (zip xs vs) ds)
               'err)])]
       [_ 'err])]))

;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (interp-env* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r ds)
       ['err 'err]
       [v (cons v (interp-env* es r ds))])]))

;; Answer -> Bool
(define (value? a)
  (match a
    ['err #f]
    [_ #t]))

;; Op1 Answer -> Answer
(define (interp-prim1 p a)
  (match (list p a)
    [(list 'add1 (? integer? i)) (add1 i)]
    [(list 'sub1 (? integer? i)) (sub1 i)]
    [(list 'zero? (? integer? i)) (zero? i)]
    [(list 'box (? value? v)) (box v)]
    [(list 'unbox (? box? b)) (unbox b)]
    [(list 'car (? pair? p)) (car p)]
    [(list 'cdr (? pair? p)) (cdr p)]
    [(list 'empty? (? value? v)) (empty? v)]
    [_ 'err]))

;; Op2 Answer Answer -> Answer
(define (interp-prim2 p a1 a2)
  (match (list p a1 a2)
    [(list '+ (? integer? i1) (? integer? i2)) (+ i1 i2)]
    [(list '- (? integer? i1) (? integer? i2)) (- i1 i2)]
    [(list 'cons (? value? v1) (? value? v2)) (cons v1 v2)]
    [_ 'err]))

;; Env Variable -> Answer
(define (lookup env x)
  (match env
    ['() 'err]
    [(cons (list y i) env)
     (match (symbol=? x y)
       [#t i]
       [#f (lookup env x)])]))

;; Env Variable Value -> Value
(define (ext r x i)
  (cons (list x i) r))

;; Defns Symbol -> Defn
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _ _) (eq? f g)])
         ds))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))
