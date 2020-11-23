#lang racket
(provide interp interp-env-heap)
(require "ast.rkt")

;; type Value* =
;; | IntegerBytes  (ends in #b000)
;; | BoxBytes      (ends in #b001)
;; | ConsBytes     (ends in #b010)
;; | TrueBytes     (#b01000)
;; | FalseBytes    (#b10000)
;; | EmptyBytes    (#b11000)

(define imm-shift        3)
(define int-shift        5)
(define imm-mask     #b111)
(define int-mask   #b11111)
(define type-int     #b000)
(define type-box     #b001)
(define type-cons    #b010)

(define val-true   (arithmetic-shift #b01 imm-shift))
(define val-false  (arithmetic-shift #b10 imm-shift))
(define val-empty  (arithmetic-shift #b11 imm-shift))

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
    [(Int i)  (cons h (arithmetic-shift i int-shift))]
    [(Bool b) (cons h (if b val-true val-false))]
    [(Empty)  (cons h val-empty)]
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


;;;;;;;;;;;;;;;;;;;;;;
;; Bit representations

(define (int-bits? v)
  (zero? (bitwise-and v int-mask)))

(define (cons-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-cons)))

(define (box-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-box)))

;;;;;;;;;;;;;
;; Primitives

;; Op1 Value* Heap -> Answer*
(define (interp-prim1 p v h)
  (match (list p v)
    [(list 'add1  (? int-bits? i))  (cons h (bits-add1 i))]
    [(list 'sub1  (? int-bits? i))  (cons h (bits-sub1 i))]
    [(list 'zero? (? int-bits? i))  (cons h (zero? i))]
    [(list 'box v)                  (alloc-box v h)]
    [(list 'unbox (? box-bits?  i)) (cons h (heap-ref h i))]
    [(list 'car   (? cons-bits? i)) (cons h (heap-ref h i))]
    [(list 'cdr   (? cons-bits? i)) (cons h (heap-ref h (+ i (arithmetic-shift 1 imm-shift))))]
    [(list 'empty? v)               (cons h (= val-empty v))]
    [_ 'err]))

;; Op2 Value* Value* Heap -> Answer*
(define (interp-prim2 p v1 v2 h)
  (match (list p v1 v2)
    [(list '+ (? int-bits? i1) (? int-bits? i2)) (cons h (+ i1 i2))]
    [(list '- (? int-bits? i1) (? int-bits? i2)) (cons h (- i1 i2))]
    [(list 'cons v1 v2)                          (alloc-cons v1 v2 h)]
    [_ 'err]))

;; IntegerBits -> IntegerBits
(define (bits-add1 i)
  (+ i (arithmetic-shift 1 imm-shift)))

(define (bits-sub1 i)
  (- i (arithmetic-shift 1 imm-shift)))

;;;;;;;;
;; Heaps

;; Value* Heap -> Answer
(define (alloc-box v h)
  (cons (cons v h)
        (bitwise-ior (arithmetic-shift (length h) imm-shift)
                     type-box)))

;; Value* Value* Heap -> Answer
(define (alloc-cons v1 v2 h)
  (cons (cons v1 (cons v2 h))
        (bitwise-ior (arithmetic-shift (length h) imm-shift)
                     type-cons)))

;; Heap Address -> Value*
(define (heap-ref h a)
  (list-ref h (arithmetic-shift a (- imm-shift))))


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
    [(? int-bits? i) (arithmetic-shift i (- int-shift))]
    [(? box-bits? i)
     (box  (unload-value (heap-ref h i) h))]
    [(? cons-bits? i)
     (cons (unload-value (heap-ref h i) h)
           (unload-value (heap-ref h (+ i (arithmetic-shift 1 imm-shift))) h))]
    [_
     (cond
       [(= v val-false) #f]
       [(= v val-true)  #t]
       [(= v val-empty) '()]
       [else (error "invalid bits")])]))

