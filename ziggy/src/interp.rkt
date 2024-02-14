#lang crook
{:= A B C D0 D0.A D1 E0 E1 F H0 H1 I J K L}
(provide interp)
{:> F} (provide interp-env)
{:> K} (provide interp-match-pat)
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
{:> L}  ;; | (Value ... -> Answer)

{:> F} ;; type Env = (Listof (List Id Value))

{:> A D0}  ;; Expr -> Integer
{:> D0 E1} ;; Expr -> Value
{:> E1 I}  ;; Expr -> Answer
{:> I}     ;; Prog -> Answer
(define (interp {:> A I} e {:> I} p)
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
                  [v (interp e2)])])
  {:> F I}
  (interp-env e '())
  {:> I}
  (match p
    [(Prog ds e)
     (interp-env e '() ds)]))

{:> F} ;; Expr Env -> Answer
{:> F}
(define (interp-env e r {:> I} ds)
  (match e
    [(Lit d) d]
    [(Eof)   eof]
    {:> H0}
    [(Empty) '()]
    {:> F L}
    [(Var x) (lookup r x)]
    {:> L}
    [(Var x) (interp-var x r ds)]
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
    {:> I L}
    [(App f es)
     (match (interp-env* es r ds)
       ['err 'err]
       [vs
        (match (defns-lookup ds f)
          [(Defn f xs e)
           ; check arity matches
           (if (= (length xs) (length vs))
               (interp-env e (zip xs vs) ds)
               'err)])])]
    {:> L}
    [(App e es)
     (match (interp-env e r ds)
       ['err 'err]
       [f
        (match (interp-env* es r ds)
          ['err 'err]
          [vs
           (if (procedure? f)
               (apply f vs)
               'err)])])]     
    {:> K}
    [(Match e ps es)
     (match (interp-env e r ds)
       ['err 'err]
       [v
        (interp-match v ps es r ds)])]
    {:> L}
    [(Lam f xs e)
     (λ vs
       ; check arity matches
       (if (= (length xs) (length vs))
           (interp-env e (append (zip xs vs) r) ds)
           'err))]))       

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

{:> L} ;; Id Env [Listof Defn] -> Answer
{:> L}
(define (interp-var x r ds)
  (match (lookup r x)
    ['err (match (defns-lookup ds x)
            [(Defn f xs e) (interp-env (Lam f xs e) '() ds)]
            [#f 'err])]
    [v v]))

{:> K} ;; Value [Listof Pat] [Listof Expr] Env Defns -> Answer
{:> K}
(define (interp-match v ps es r ds)
  (match* (ps es)
    [('() '()) 'err]
    [((cons p ps) (cons e es))
     (match (interp-match-pat p v r)
       [#f (interp-match v ps es r ds)]
       [r  (interp-env e r ds)])]))

{:> K} ;; Pat Value Env -> [Maybe Env]
{:> K}
(define (interp-match-pat p v r)
  (match p
    [(Var '_) r]
    [(Var x) (ext r x v)]
    [(Lit l) (and (eqv? l v) r)]
    [(Box p)
     (match v
       [(box v)
        (interp-match-pat p v r)]
       [_ #f])]
    [(Cons p1 p2)
     (match v
       [(cons v1 v2)
        (match (interp-match-pat p1 v1 r)
          [#f #f]
          [r1 (interp-match-pat p2 v2 r1)])]
       [_ #f])]
    [(Conj p1 p2)
     (match (interp-match-pat p1 v r)
       [#f #f]
       [r1 (interp-match-pat p2 v r1)])]))

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

{:> F L} ;; Env Id -> Value
{:> F L}
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))

{:> L} ;; Env Id -> Answer
{:> L}
(define (lookup env x)
  (match env
    ['() 'err]
    [(cons (list y i) env)
     (match (symbol=? x y)
       [#t i]
       [#f (lookup env x)])]))

{:> F} ;; Env Id Value -> Env
{:> F}
(define (ext r x v)
  (cons (list x v) r))
