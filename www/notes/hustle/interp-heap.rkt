#lang racket
(provide interp interp-env-heap)
(require "ast.rkt")

;; type AAnswer = AValue | 'err
;; type Answer  = Value | 'err

;; type AValue =
;; | Integer
;; | Boolean
;; | '()
;; | (list 'box  Address)
;; | (list 'cons Address)

;; type Heap = (Vectorof AValue)

;; type REnv = (Listof (List Id AValue))

;; heap size in "words"
(define heap-size 10000) 

;; Expr -> Answer
(define (interp e)
  (let ((h (make-vector heap-size)))
    (unload (interp-env-heap e '() h) h)))

;; AAnswer Heap -> Answer
(define (unload v h)
  (match v
    [(? integer? i) i]
    [(? boolean? b) b]
    ['() '()]
    [(list 'box a) (box (unload (vector-ref h a) h))]
    [(list 'cons a) (cons (unload (vector-ref h a) h)
                          (unload (vector-ref h (add1 a)) h))]))

;; Expr REnv Heap -> Integer
(define (interp-env-heap e r h)
  (local [;; Memory management
          (define a (box 0))

          ;; Value -> Address
          (define (alloc-box v)
            (let ((i (unbox a)))
              (vector-set! h i v)
              (set-box! a (add1 i))
              (list 'box i)))

          ;; Value Value -> Address
          (define (alloc-cons v1 v2)
            (let ((i (unbox a)))
              (vector-set! h (+ i 0) v1)
              (vector-set! h (+ i 1) v1)
              (set-box! a (+ i 2))
              (list 'cons i)))
          
          ;; Expr Env -> Answer
          (define (interp-env e r)
            (match e
              [(Var x) (lookup r x)]
              [(Int i) i]
              [(Bool b) b]
              [(Empty) '()]
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
              [(list 'box (? value? v))     (alloc-box v)]
              [(list 'unbox (list 'box i))  (vector-ref h i)]
              [(list 'car   (list 'cons i)) (vector-ref h i)]
              [(list 'cdr   (list 'cons i)) (vector-ref h (add1 i))]              
              [(list 'empty? (? value? v)) (empty? v)]
              [_ 'err]))

          ;; Op2 Answer Answer -> Answer
          (define (interp-prim2 p a1 a2)
            (match (list p a1 a2)
              [(list '+ (? integer? i1) (? integer? i2)) (+ i1 i2)]
              [(list '- (? integer? i1) (? integer? i2)) (- i1 i2)]
              [(list 'cons (? value? v1) (? value? v2)) (alloc-cons v1 v2)]
              [_ 'err]))]
    
    (interp-env e r)))

;; Answer -> Bool
(define (value? a)
  (match a
    ['err #f]
    [_ #t]))

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
