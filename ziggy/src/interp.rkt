#lang crook
{:= A B C D0 D1 E0 E1 F}
(provide interp)
(require "ast.rkt")
{:> B} (require "interp-prim.rkt")

{:> D0} ;; type Value =
{:> D0} ;; | Integer
{:> D0} ;; | Boolean
{:> D1} ;; | Character
{:> E0} ;; | Eof
{:> E0} ;; | Void

{:> F} ;; type Env = (Listof (List Id Value))

{:> A D0}  ;; Expr -> Integer
{:> D0 E1} ;; Expr -> Value
{:> E1}    ;; Expr -> Answer
(define (interp e)
  {:> F}
  (interp-env e '())
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
(define (interp-env e r)
  (match e
    [(Lit d) d]
    [(Eof)   eof]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]
    [(Let x e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v (interp-env e2 (ext r x v))])]))

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

