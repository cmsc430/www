#lang racket
(provide (all-defined-out))

;; type Expr =
;; | Integer
;; | Boolean
;; | Character
;; | String
;; | Symbol
;; | Variable
;; | '()
;; | `(box ,Expr)
;; | `(if ,Expr ,Expr ,Expr)
;; | `(let ,(List Variable Expr) ... ,Expr)
;; | `(letrec ,(List Variable Lambda) ... ,Expr)
;; | `(λ ,Bindings ,Expr)
;; | `(apply ,Expr ,Expr)
;; | `(,Prim ,Expr ...)
;; | `(,Expr ,Expr ...)

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | String
;; | Symbol
;; | '()
;; | (Box Value)
;; | (Cons Value Value)
;; | Function

;; type Function =
;; | (Values ... -> Answer)

;; type Answer = Value | 'err

;; type REnv = (Listof (List Variable Value))

;; Expr REnv -> Answer
(define (interp-env e r)
  (match e
    ;; produce fresh strings each time a literal is eval'd
    [(? string? s) (string-copy s)]
    [(? value? v) v]
    [''() '()]
    [`',(? symbol? s) s]    
    [`(if ,e0 ,e1 ,e2)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(? symbol? x)
     (lookup r x)]
    [`(let (,`(,xs ,es) ...) ,e)
     (match (interp-env* es r)
       ['err 'err]
       [vs
        (interp-env e (append (zip xs vs) r))])]
    [`(letrec (,`(,xs ,es) ...) ,e)
     (letrec ((r* (λ ()
                    (append
                     (zip xs
                          (map (λ (l) (λ vs (apply (interp-env l (r*)) vs)))
                               es))
                     r))))
       (interp-env e (r*)))]
    [`(λ (,xs ...) ,e)
     (λ vs
       (if (= (length vs) (length xs))
           (interp-env e (append (zip xs vs) r))
           'err))]
    [`(λ (,xs ... . ,x) ,e)
     (λ vs
       (if (>= (length vs) (length xs))
           (interp-env e (append (zip/remainder xs vs x) r))
           'err))]
    [`(apply ,e0 ,e1)
     (let ((v0 (interp-env e0 r))
           (vs (interp-env e1 r)))
       (if (list? vs)
           (apply v0 vs)
           'err))]
    [`(,(? prim? p) ,es ...)
     (let ((as (interp-env* es r)))
       (interp-prim p as))]
    [`(,e ,es ...)
     (match (interp-env* (cons e es) r)
       [(list f vs ...)
        (if (procedure? f)
            (apply f vs)
            'err)]
       ['err 'err])]
    [_ 'err]))

;; (Listof Expr) REnv (Listof Defn) -> (Listof Value) | 'err
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
       (memq x '(add1 sub1 zero? abs - char? boolean? integer? integer->char char->integer
                      string? box? empty? cons cons? box unbox car cdr string-length
                      make-string string-ref = < <= char=? boolean=? + eq? gensym symbol?
                      procedure?))))

;; Any -> Boolean
(define (value? x)
  (or (integer? x)
      (boolean? x)
      (char? x)
      (string? x)))

;; Prim (Listof Answer) -> Answer
(define (interp-prim p as)
  (match (cons p as)
    [(list p (? value?) ... 'err _ ...) 'err]
    [(list '- (? integer? i0)) (- i0)]
    [(list '- (? integer? i0) (? integer? i1)) (- i0 i1)]
    [(list 'abs (? integer? i0)) (abs i0)]
    [(list 'add1 (? integer? i0)) (+ i0 1)]
    [(list 'sub1 (? integer? i0)) (- i0 1)]
    [(list 'zero? (? integer? i0)) (zero? i0)]
    [(list 'char? v0) (char? v0)]
    [(list 'integer? v0) (integer? v0)]
    [(list 'boolean? v0) (boolean? v0)]
    [(list 'integer->char (? codepoint? i0)) (integer->char i0)]
    [(list 'char->integer (? char? c)) (char->integer c)]
    [(list '+ (? integer? i0) (? integer? i1)) (+ i0 i1)]
    [(list 'cons v0 v1) (cons v0 v1)]
    [(list 'car (? cons? v0)) (car v0)]
    [(list 'cdr (? cons? v0)) (cdr v0)]
    [(list 'string? v0) (string? v0)]
    [(list 'box? v0) (box? v0)]
    [(list 'empty? v0) (empty? v0)]
    [(list 'cons? v0) (cons? v0)]
    [(list 'cons v0 v1) (cons v0 v1)]
    [(list 'box v0) (box v0)]
    [(list 'unbox (? box? v0)) (unbox v0)]
    [(list 'string-length (? string? v0)) (string-length v0)]
    [(list 'make-string (? natural? v0) (? char? v1)) (make-string v0 v1)]
    [(list 'string-ref (? string? v0) (? natural? v1))
     (if (< v1 (string-length v0))
         (string-ref v0 v1)
         'err)]
    [(list '= (? integer? v0) (? integer? v1)) (= v0 v1)]
    [(list '< (? integer? v0) (? integer? v1)) (< v0 v1)]
    [(list '<= (? integer? v0) (? integer? v1)) (<= v0 v1)]
    [(list 'char=? (? char? v0) (? char? v1)) (char=? v0 v1)]
    [(list 'boolean=? (? boolean? v0) (? boolean? v1)) (boolean=? v0 v1)]
    [(list 'eq? v0 v1) (eq? v0 v1)]
    [(list 'gensym) (gensym)]
    [(list 'symbol? v0) (symbol? v0)]
    [(list 'procedure? v0) (procedure? v0)]      
    [_ 'err]))

;; REnv Variable -> Answer
(define (lookup env x)
  (match env
    ['() 'err]
    [(cons (list y v) env)
     (match (symbol=? x y)
       [#t v]
       [#f (lookup env x)])]))

;; REnv Variable Value -> Value
(define (ext r x v)
  (cons (list x v) r))

;; Any -> Boolean
(define (codepoint? x)
  (and (integer? x)
       (<= 0 x #x10FFFF)
       (not (<= #xD800 x #xDFFF))))

;; (Listof A) (Listof B) -> (Listof (List A B))
(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y) (zip xs ys))]))

;; like zip but ys can be longer and remainder is associated with r
(define (zip/remainder xs ys r)
  (match* (xs ys)
    [('() ys) (list (list r ys))]
    [((cons x xs) (cons y ys))
     (cons (list x y) (zip/remainder xs ys r))]))

    
