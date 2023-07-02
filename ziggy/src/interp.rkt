#lang crook
{:= A B C D0 D1 E0 E1 F H0 H1 I J}
(provide interp)
(require "ast.rkt")
{:> B} (require "interp-prim.rkt")

{:> D0} ;; type Value =
{:> D0} ;; | Integer
{:> D0} ;; | Boolean
{:> D1} ;; | Character
{:> E0} ;; | Eof
{:> E0} ;; | Void
{:> H0} ;; | '()
{:> H0} ;; | (cons Value Value)
{:> H0} ;; | (box Value)
{:> H1} ;; | (string Character ...)
{:> H1} ;; | (vector Value ...)

{:> F} ;; type Env = (Listof (List Id Value))

{:> A D0}  ;; Expr -> Integer
{:> D0 E1} ;; Expr -> Value
{:> E1 I}  ;; Expr -> Answer
{:> I}     ;; Prog -> Answer
(define (interp {:> A I} e {:> I} p)
  {:> F I}
  (interp-env e '())
  {:> I}
  (match p
    [(Prog ds e)
     (interp-env e '() ds)])
  {:> A F}
  (match e
    {:> A D0}  [(Lit i) i]
    {:> D0}    [(Lit d) d]
    {:> E0}    [(Eof)   eof]
    {:> E0}    [(Prim0 p)
                (interp-prim0 p)]
    {:> B E1}  [(Prim1 p e)
                (interp-prim1 p (interp e))]
    {:> E1}    [(Prim1 p e)
                (match (interp e)
                  ['err 'err]
                  [v (interp-prim1 p v)])]
    {:> C D0}  [(IfZero e1 e2 e3)
                (if (zero? (interp e1))
                    (interp e2)
                    (interp e3))]
    {:> D0 E1} [(If e1 e2 e3)
                (if (interp e1)
                    (interp e2)
                    (interp e3))]
    {:> E1}    [(If e1 e2 e3)
                (match (interp e1)
                  ['err 'err]
                  [v (if v
                         (interp e2)
                         (interp e3))])]
    {:> E0 E1} [(Begin e1 e2)
                (begin (interp e1)
                       (interp e2))]
    {:> E1}    [(Begin e1 e2)
                (match (interp e1)
                  ['err 'err]
                  [v (interp e2)])]))

{:> F} ;; Expr Env -> Answer
{:> F}
(define (interp-env e r {:> I} ds)
  (match e
    [(Lit d) d]
    [(Eof)   eof]
    {:> H0}
    [(Empty) '()]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r {:> I} ds)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r {:> I} ds)
       ['err 'err]
       [v1 (match (interp-env e2 r {:> I} ds)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    {:> H1}
    [(Prim3 p e1 e2 e3)
     (match (interp-env e1 r {:> I} ds)
       ['err 'err]
       [v1 (match (interp-env e2 r {:> I} ds)
             ['err 'err]
             [v2 (match (interp-env e3 r {:> I} ds)
                   ['err 'err]
                   [v3 (interp-prim3 p v1 v2 v3)])])])]
    [(If e0 e1 e2)
     (match (interp-env e0 r {:> I} ds)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r {:> I} ds)
            (interp-env e2 r {:> I} ds))])]
    [(Begin e1 e2)
     (match (interp-env e1 r {:> I} ds)
       ['err 'err]
       [v    (interp-env e2 r {:> I} ds)])]
    [(Let x e1 e2)
     (match (interp-env e1 r {:> I} ds)
       ['err 'err]
       [v (interp-env e2 (ext r x v) {:> I} ds)])]
    {:> I}
    [(App f es)
     (match (interp-env* es r ds)
       ['err 'err]
       [vs
        (match (defns-lookup ds f)
          [(Defn f xs e)
           ; check arity matches
           (if (= (length xs) (length vs))
               (interp-env e (zip xs vs) ds)
               'err)])])]))   

{:> I} ;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
{:> I}
(define (interp-env* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r ds)
       ['err 'err]
       [v (match (interp-env* es r ds)
            ['err 'err]
            [vs (cons v vs)])])]))

{:> I} ;; Defns Symbol -> Defn
{:> I}
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _ _) (eq? f g)])
         ds))

{:> I}
(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))

{:> F} ;; Env Id -> Value
{:> F}
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))

{:> F} ;; Env Id Value -> Env
{:> F}
(define (ext r x v)
  (cons (list x v) r))

