#lang racket
(provide (all-defined-out))


;; type Expr =
;; ...
;; | `(λ ,(Listof Variable) ,Expr)

;; type Value =
;; ...
;; | ((Listof Value) -> Answer)

;; Expr REnv -> Answer
(define (interp-env e r)
  (match e
    [''() '()]
    [(? syntactic-value? v) v]
    [(list (? prim? p) es ...)
     (match (interp-env* es r)
       [(list vs ...) (interp-prim p vs)]
       [_ 'err])]
    [`(if ,e0 ,e1 ,e2)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(? symbol? x)
     (lookup r x)]
    [`(let ((,x ,e0)) ,e1)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (interp-env e1 (ext r x v))])]

    [`(λ (,xs ...) ,e)

     (λ (vs) (interp-env e (append (zip xs vs) r)))]
    
    [`(,e . ,es)
     (match (interp-env* (cons e es) r)
       [(list f vs ...) (f vs)]
       [_ 'err])]))


;; (Listof Expr) REnv -> (Listof Value) | 'err
(define (interp-env* es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (cons v (interp-env* es r))])]))


(define Y '(λ (t) ((λ (f) (t (λ (z) ((f f) z))))
                   (λ (f) (t (λ (z) ((f f) z)))))))

(define Tri `(,Y
               (λ (tri)
                 (λ (n)
                   (if (zero? n)
                       1
                       (+ n (tri (sub1 n))))))))

;; type Prog =
;; | `(begin ,@(Listof Defn) ,Expr)
;; | Expr

;; type Defn = `(define (,Variable ,@(Listof Variable)) ,Expr)


;; Prog -> Answer
(define (interp p)
  (match p
    [(list 'begin ds ... e)
     (interp-env e (interp-defns ds))]
    [e (interp-env e '())]))

;; (Listof Defn) -> REnv
(define (interp-defns ds)
  (map (lambda (d)
         (match d
           [`(define (,f . ,xs) ,e)
            (list f (interp-defn d ds))]))
       ds))

;; Defn (Listof Defn) -> ((Listof Value) -> Answer)
(define (interp-defn d ds)
  (match d
    [`(define (,f . ,xs) ,e)
     (lambda (vs)
       (if (= (length vs) (length xs))
           (interp-env e (append (interp-defns ds) (zip xs vs)))
           'err))]))



;; type Value =
;; ...
;; | `(closure ,(Listof Variable) ,Expr ,Env)

(define (interp-closure e)
  (interp-env-closure e '()))

;; Expr REnv -> Answer
(define (interp-env-closure e r)
  (match e
    [''() '()]
    [(? syntactic-value? v) v]
    [(list (? prim? p) es ...)
     (match (interp-env-closure* es r)
       [(list vs ...) (interp-prim p vs)]
       [_ 'err])]
    [`(if ,e0 ,e1 ,e2)
     (match (interp-env-closure e0 r)
       ['err 'err]
       [v
        (if v
            (interp-env-closure e1 r)
            (interp-env-closure e2 r))])]
    [(? symbol? x)
     (lookup r x)]
    [`(let ((,x ,e0)) ,e1)
     (match (interp-env-closure e0 r)
       ['err 'err]
       [v
        (interp-env-closure e1 (ext r x v))])]

    [`(λ (,xs ...) ,e)
     `(closure ,xs ,e ,r)]    
    
    [`(,e . ,es)
     (match (interp-env-closure* (cons e es) r)
       [(list `(closure ,xs ,e ,r) vs ...)
        (if (= (length vs) (length xs))
           (interp-env-closure e (append (zip xs vs) r))
           'err)]
       [_ 'err])]))



;; (Listof Expr) REnv -> (Listof Value) | 'err
(define (interp-env-closure* es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env-closure e r)
       ['err 'err]
       [v (cons v (interp-env-closure* es r))])]))






;; Any -> Boolean
(define (prim? x)
  (and (symbol? x)
       (memq x '(add1 sub1 + - zero?
                      box unbox empty? cons car cdr))))

;; Any -> Boolean
(define (syntactic-value? x)
  (or (integer? x)
      (boolean? x)
      (null? x)))

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
