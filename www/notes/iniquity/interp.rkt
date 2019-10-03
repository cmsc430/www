#lang racket
(provide (all-defined-out))

;; type Prog =
;; | `(begin ,@(Listof Defn) ,Expr)
;; | Expr

;; type Defn = `(define (,Variable ,@(Listof Variable)) ,Expr)

;; Prog -> Answer
(define (interp p)
  (match p
    [(list 'begin ds ... e)
     (interp-env e '() ds)]
    [e (interp-env e '() '())]))

;; Expr REnv (Listof Defn) -> Integer
(define (interp-env e r ds)
  (match e
    [''() '()]
    [(? value? v) v]
    [(list (? prim? p) es ...)
     (let ((as (interp-env* es r ds)))
       (interp-prim p as))]
    [`(if ,e0 ,e1 ,e2)
     (match (interp-env e0 r ds)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r ds)
            (interp-env e2 r ds))])]    
    [(? symbol? x)
     (lookup r x)]
    [`(let ((,x ,e0)) ,e1)
     (match (interp-env e0 r ds)
       ['err 'err]
       [v
        (interp-env e1 (ext r x v) ds)])]
    
    [(list (? (defns-lookup ds) f) es ...)
     (match (interp-env* es r ds)
       [(list vs ...)
        (match ((defns-lookup ds) f)
          [`(define (,f ,xs ...) ,e)
           ; check arity matches
           (if (= (length xs) (length vs))
               (interp-env e (zip xs vs) ds)
               'err)])]
       [_ 'err])]))

;; (Listof Defn) -> (Symbol -> Defn)
(define (defns-lookup ds)
  (λ (f)
    (findf (λ (d) (eq? (first (second d)) f))
           ds)))

;; [Listof Expr] REnv -> [Listof Answer]
(define (interp-env* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (cons (interp-env e r ds)
           (interp-env* es r ds))]))

;; Any -> Boolean
(define (prim? x)
  (and (symbol? x)
       (memq x '(add1 sub1 + - zero?
                      box unbox empty? cons car cdr))))

;; Any -> Boolean
(define (value? x)
  (or (integer? x)
      (boolean? x)
      (null? x)
      (and (pair? x)
           (value? (car x))
           (value? (cdr x)))))

;; Prim [Listof Answer] -> Answer
(define (interp-prim p as)
  (match (cons p as)
    [(list p (? value?) ... 'err _ ...) 'err]
    [(list 'add1 (? integer? i0)) (+ i0 1)]
    [(list 'sub1 (? integer? i0)) (- i0 1)]
    [(list 'zero? (? integer? i0)) (zero? i0)]
    [(list '+ (? integer? i0) (? integer? i1)) (+ i0 i1)]
    [(list '- (? integer? i0) (? integer? i1)) (- i0 i1)]
    [(list 'box v0) (box v0)]
    [(list 'unbox (? box? v0)) (unbox v0)]
    [(list 'empty? v0) (empty? v0)]
    [(list 'cons v0 v1) (cons v0 v1)]
    [(list 'car (cons v0 v1)) v0]
    [(list 'cdr (cons v0 v1)) v1]
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
