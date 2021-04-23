#lang racket
(provide interp interp-env interp-prim1)
(require "ast.rkt"
         "env.rkt"
         "interp-prims.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | (Fun f)
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)

;; type REnv = (Listof (List Id Value))
;; type Defns = (Listof Defn)

;; Prog Defns -> Answer
(define (interp p)
  (match p
    [(Prog ds e)
     (interp-env e '() ds)]))

;; Expr Env Defns -> Answer
(define (interp-env e r ds)
  (match e
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof)    eof]
    [(Empty)  '()]
    [(Var x)  (lookup r x)]
    [(Prim0 'void) (void)]
    [(Prim0 'read-byte) (read-byte)]
    [(Prim0 'peek-byte) (peek-byte)]
    [(Prim1 p e)
     (match (interp-env e r ds)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v1 (match (interp-env e2 r ds)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(If p e1 e2)
     (match (interp-env p r ds)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r ds)
            (interp-env e2 r ds))])]
    [(Begin e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [_ (interp-env e2 r ds)])]
    [(Let x e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v (interp-env e2 (ext r x v) ds)])]
    [(App f es)
     (match (interp-env* es r ds)
      [(list vs ...)
        (match (defns-lookup ds f)
         [(Defn f xs body)
          ; arity check
          (if (= (length vs) (length xs))
              (interp-env body (zip xs vs) ds)
              'err)])])]
    [(Fun f)
      (match (defns-lookup ds f)
        [(Defn f xs body)
         (lambda (es r)
          (match (interp-env* es r ds)
            [(list vs ...)
             (if (= (length vs) (length xs))
                 (interp-env body (zip xs vs) ds)
                 'err)]))]
        [_ 'err])]
    [(FCall f es)
      (match (interp-env f r ds)
       [(? procedure? f) (f es r)]
       [_ 'err])]
    [_         'err]))

;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (interp-env* es r ds)
  (match es
    ['() '()]
    [(cons e es)
      (match (interp-env e r ds)
       ['err 'err]
       [v (cons v (interp-env* es r ds))])]))

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
