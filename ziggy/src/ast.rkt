#lang crook
{:= A B C D0 D1}
(provide {:> A} Lit {:> B} Prim1 {:> C D0} IfZero {:> D0} If)

{:> A D0} ;; type Expr = (Lit Integer)
{:> D0}   ;; type Expr = (Lit Datum)
{:> B}    ;;           | (Prim1 Op Expr)
{:> C D0} ;;           | (IfZero Expr Expr Expr)
{:> D0}   ;;           | (If Expr Expr Expr)
{:> D0}   ;; type Datum = Integer
{:> D0}   ;;            | Boolean
{:> D1}   ;;            | Char
{:> B}    ;; type Op = 'add1 | 'sub1
{:> D0}   ;;         | 'zero?
{:> A D0} (struct Lit (i) #:prefab)
{:> D0}   (struct Lit (d) #:prefab)
{:> B}    (struct Prim1 (p e) #:prefab)
{:> C D0} (struct IfZero (e1 e2 e3) #:prefab)
{:> D0}   (struct If (e1 e2 e3) #:prefab)
