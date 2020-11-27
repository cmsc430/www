#lang racket
(provide (all-defined-out))
(require "ast.rkt" "syntax.rkt")

;; type Expr =
;; ...
;; | `(λ ,(Listof Variable) ,Expr)

;; type Value =
;; ...
;; | Function

;; type Function =
;; | (Values ... -> Answer)

(define (interp p)
  (match (desugar-prog p)
    [(prog _ e)
      (interp-env e '())]))

;; Expr REnv -> Answer
(define (interp-env e r)
  (match e
    [(nil-e) '()]
    [(int-e i) i]
    [(bool-e b) b]
    [(prim-e p es)
     (match (interp-env* es r)
       [(list vs ...) (interp-prim p vs)]
       [_ 'err])]
    [(if-e e0 e1 e2)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(var-e x)
     (lookup r x)]
    [(let-e (list (binding x e0)) e1)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (interp-env e1 (ext r x v))])]    
    [(letr-e bs e)
     (letrec ((r* (λ ()
                    (append
                     (zip (get-vars bs)
                          ;; η-expansion to delay evaluating r*
                          ;; relies on RHSs being functions
                          (map (λ (l) (λ vs (apply (interp-env l (r*)) vs)))
                               (get-defs bs)))
                     r))))
       (interp-env e (r*)))]
    [(lam-e xs e)
     (λ vs
       (if (= (length vs) (length xs))
           (interp-env e (append (zip xs vs) r))
           'err))]
    [(app-e e es)
     (match (interp-env* (cons e es) r)
       [(list f vs ...)
        (if (procedure? f)
            (apply f vs)
            'err)]
       [_ 'err])]))

;; (Listof Expr) REnv -> (Listof Value) | 'err
(define (interp-env* es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (cons v (interp-env* es r))])]))

;; Any -> Boolean
(define (prim? x)
  (and (symbol? x)
       (memq x '(add1 sub1 + - zero?
                      box unbox empty? cons car cdr))))

;; Any -> Boolean
(define (syntactic-value? x)
  (or (int-e? x)
      (bool-e? x)
      (nil-e? x)))

;; Prim (Listof Value) -> Answer
(define (interp-prim p vs)
  (match (cons p vs)
    [(list 'add1 (? integer? i0))  (add1 i0)]
    [(list 'sub1 (? integer? i0))  (sub1 i0)]
    [(list 'zero? (? integer? i0)) (zero? i0)]
    [(list 'box v0)                (box v0)]
    [(list 'unbox (? box? v0))     (unbox v0)]
    [(list 'empty? v0)             (empty? v0)]
    [(list 'cons v0 v1)            (cons v0 v1)]
    [(list 'car (cons v0 v1))      v0]
    [(list 'cdr (cons v0 v1))      v1]
    [(list '+ (? integer? i0) (? integer? i1))
     (+ i0 i1)]
    [(list '- (? integer? i0) (? integer? i1))
     (- i0 i1)]
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

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))
