#lang crook
{:= A B C D0 D1}
(provide interp)
(require "ast.rkt")
{:> B} (require "interp-prim.rkt")

{:> D0} ;; type Value =
{:> D0} ;; | Integer
{:> D0} ;; | Boolean
{:> D1} ;; | Character

{:> A D0} ;; Expr -> Integer
{:> D0}   ;; Expr -> Value
(define (interp e)
  (match e
    {:> A D0}
    [(Lit i) i]
    {:> D0}
    [(Lit d) d]
    {:> B}
    [(Prim1 p e)
     (interp-prim1 p (interp e))]
    {:> C D0}
    [(IfZero e1 e2 e3)
     (if (zero? (interp e1))
         (interp e2)
         (interp e3))]
    {:> D0}
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]))
