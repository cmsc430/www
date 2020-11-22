#lang racket
(provide interp interp-env-heap)
(require "ast.rkt")

;; type Answer* =
;; | (cons Heap Value*)
;; | 'err
;; type Value* =
;; | Integer
;; | Boolean
;; | '()
;; | (list 'box  Address)
;; | (list 'cons Address)
;; type Heap = (Listof Value*)
;; type REnv = (Listof (List Id Value*))

;; type Answer = Value | 'err
;; type Value =
;; | Integer
;; | Boolean
;; | '()
;; | (box Value)
;; | (cons Value Value)


;; Expr -> Answer
(define (interp e)  
  (unload (interp-env-heap e '() '())))

;; Expr REnv Heap -> Answer
(define (interp-env-heap e r h)
  (match e
    [(Var x)  (cons h (lookup r x))]
    [(Int i)  (cons h i)]
    [(Bool b) (cons h b)]
    [(Empty)  (cons h '())]
    [(Prim1 p e)
     (match (interp-env-heap e r h)
       ['err 'err]
       [(cons h a)
        (interp-prim1 p a h)])]
    [(Prim2 p e1 e2)
     (match (interp-env-heap e1 r h)
       ['err 'err]
       [(cons h a1)        
        (match (interp-env-heap e2 r h)
          ['err 'err]
          [(cons h a2)
           (interp-prim2 p a1 a2 h)])])]
    [(If p e1 e2)
     (match (interp-env-heap p r h)
       ['err 'err]
       [(cons h v)
        (if v
            (interp-env-heap e1 r h)
            (interp-env-heap e2 r h))])]
    [(Let x e1 e2)
     (match (interp-env-heap e1 r h)
       ['err 'err]
       [(cons h v)
        (interp-env-heap e2 (ext r x v) h)])]))


;;;;;;;;;;;;;
;; Primitives

;; Op1 Value* Heap -> Answer*
(define (interp-prim1 p v h)
  (match (list p v)
    [(list 'add1 (? integer? i))  (cons h (add1 i))]
    [(list 'sub1 (? integer? i))  (cons h (sub1 i))]
    [(list 'zero? (? integer? i)) (cons h (zero? i))]
    [(list 'box v)                (alloc-box v h)]
    [(list 'unbox (list 'box i))  (cons h (heap-ref h i))]
    [(list 'car   (list 'cons i)) (cons h (heap-ref h i))]
    [(list 'cdr   (list 'cons i)) (cons h (heap-ref h (add1 i)))]
    [(list 'empty? v)             (cons h (empty? v))]
    [_ 'err]))

;; Op2 Value* Value* Heap -> Answer*
(define (interp-prim2 p v1 v2 h)
  (match (list p v1 v2)
    [(list '+ (? integer? i1) (? integer? i2)) (cons h (+ i1 i2))]
    [(list '- (? integer? i1) (? integer? i2)) (cons h (- i1 i2))]
    [(list 'cons v1 v2)                        (alloc-cons v1 v2 h)]
    [_ 'err]))


;;;;;;;;
;; Heaps

;; Value* Heap -> Answer
(define (alloc-box v h)
  (cons (cons v h)
        (list 'box (length h))))

;; Value* Value* Heap -> Answer
(define (alloc-cons v1 v2 h)
  (cons (cons v1 (cons v2 h))
        (list 'cons (length h))))

;; Heap Address -> Value*
(define (heap-ref h a)
  (list-ref h a))


;;;;;;;;;;;;;;;
;; Environments

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

;;;;;;;;;;;;
;; Unloading

;; Answer* -> Answer
(define (unload a)
  (match a
    ['err 'err]
    [(cons h v) (unload-value v h)]))

;; Value* Heap -> Value
(define (unload-value v h)
  (match v
    [(? integer? i) i]
    [(? boolean? b) b]
    ['() '()]
    [(list 'box a)  (box (unload-value (heap-ref h a) h))]
    [(list 'cons a) (cons (unload-value (heap-ref h a) h)
                          (unload-value (heap-ref h (add1 a)) h))]))
