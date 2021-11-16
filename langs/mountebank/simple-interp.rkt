#lang racket

;; type Expr = Number
;;           | Boolean
;;           | (list Op1 Expr)
;;           | (list Op2 Expr)
;;           | (list 'if Expr Expr Expr)
;;           | (list Expr Expr)
;;           | (list 'λ (list Id) Expr)
;;           | Id

;; type Id = Symbol
;; type Op1 = 'sub1 | 'zero?
;; type Op2 = '+

;; type Value = Number
;;            | Boolean
;;            | (Value -> Value)

;; Expr Env -> Value
(define (interp e r)
  (match e
    [(list '+ e1 e2)
     (+ (interp e1 r) (interp e2 r))]
    [(list 'sub1 e1)
     (sub1 (interp e1 r))]
    [(list 'zero? e1)
     (zero? (interp e1 r))]
    [(list 'if e1 e2 e3)
     (if (interp e1 r)
         (interp e2 r)
	 (interp e3 r))]
    [(list 'λ (list x) e1)
     (λ (v) (interp e1 (cons (cons x v) r)))]
    [(list e1 e2)
     ((interp e1 r) (interp e2 r))]
    [_
     (if (symbol? e)
         (lookup e r)
	 e)]))

;; Id Env -> Value
(define (lookup x r)
  (match r
    [(cons (cons y v) r)
     (if (eq? x y)
         v
	 (lookup x r))]))

(interp '(((λ (t)
            ((λ (f) (t (λ (z) ((f f) z))))
             (λ (f) (t (λ (z) ((f f) z))))))
           (λ (tri)
            (λ (n)
             (if (zero? n)
                 0
                 (+ n (tri (sub1 n)))))))
          36)
          '())
