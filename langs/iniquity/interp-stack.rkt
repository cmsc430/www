#lang racket
(provide interp interp-env)
(require "ast.rkt"
         "env.rkt"
         "interp-prims-stack.rkt"
         "max-stack.rkt")

(define *stack-limit* 10000)

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)
;; | (vector Value ...)
;; | (string Char ...)

;; type CEnv = (Listof Id)
;; type Stack = (Listof Value)
;; type Defns = (Listof Defn)

;; Prog -> Answer
(define (interp p)
  (match p
    [(Prog ds e)
     (if (>= (max-stack e) *stack-limit*)
         'err
         (interp-env e '() ds '()))]))

(define (lookup-address r x)
  (match r
    ['() (error "unbound variable")]
    [(cons y r)
     (if (eq? x y)
         0
         (add1 (lookup-address r x)))]))

;; Expr CEnv Defns Stack -> Answer
(define (interp-env e r ds s)
  (printf "stack: ~a~n" s)
  (match e
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof)    eof]
    [(Empty)  '()]
    [(Var x)  (list-ref s (lookup-address r x))]
    [(Str s)  s]
    [(Prim0 'void) (void)]
    [(Prim0 'read-byte) (read-byte)]
    [(Prim0 'peek-byte) (peek-byte)]
    [(Prim1 p e)
     (match (interp-env e r ds s)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r ds s)
       ['err 'err]
       [v1 (match (interp-env e2 (cons #f r) ds (cons v1 s))
             ['err 'err]
             [v2 (interp-prim2 p v2 (cons v1 s))])])]
    [(Prim3 p e1 e2 e3)
     (match (interp-env e1 r ds s)
       ['err 'err]
       [v1 (match (interp-env e2 (cons #f r) ds (cons v1 s))
             ['err 'err]
             [v2 (match (interp-env e3 (cons #f (cons #f r)) ds (cons v2 (cons v1 s)))
                   ['err 'err]
                   [v3 (interp-prim3 p v3 (cons v2 (cons v1 s)))])])])]
    [(If p e1 e2)
     (match (interp-env p r ds s)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r ds s)
            (interp-env e2 r ds s))])]
    [(Begin e1 e2)
     (match (interp-env e1 r ds s)
       ['err 'err]
       [_    (interp-env e2 r ds s)])]
    [(Let x e1 e2)
     (match (interp-env e1 r ds s)
       ['err 'err]
       [v (interp-env e2 (cons x r) ds (cons v s))])]
    [(App f es)
     (match (interp-env* es (cons #f r) ds (cons 'return s))
       ['err 'err]
       [vs
        (match (defns-lookup ds f)
          [(Defn f xs e)
           ; check arity matches
           (if (= (length xs) (length es))
               (begin (printf "~a\n" (+ (length (append (cons #f vs) s)) (max-stack e)))
                      (if (>= (+ (length (append (cons #f vs) s)) (max-stack e)) *stack-limit*)
                          'err
                          (interp-env e xs ds (append vs (list 'return) s))))
               'err)])])]))

;; (Listof Expr) CEnv Defns Stack -> Stack | 'err
(define (interp-env* es r ds s)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r ds s)
       ['err 'err]
       [v (match (interp-env* es (cons #f r) ds (cons v s))
            ['err 'err]
            [vs (cons v vs)])])]))

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
