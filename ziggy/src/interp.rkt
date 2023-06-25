#lang crook
{:= A B C D0 D1 E0 E1}
(provide interp)
(require "ast.rkt")
{:> B} (require "interp-prim.rkt")

{:> D0} ;; type Value =
{:> D0} ;; | Integer
{:> D0} ;; | Boolean
{:> D1} ;; | Character
{:> E0} ;; | Eof
{:> E0} ;; | Void

{:> A D0}  ;; Expr -> Integer
{:> D0 E1} ;; Expr -> Value
{:> E1}    ;; Expr -> Answer
(define (interp e)
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
