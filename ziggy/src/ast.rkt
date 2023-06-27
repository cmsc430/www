#lang crook
{:= A B C D0 D1 E0 E1 F H0}
(provide {:> A} Lit {:> E0} Prim0 {:> B} Prim1 {:> F} Prim2 {:> C D0} IfZero {:> D0} If {:> E0} Eof {:> E0} Begin {:> F} Let {:> F} Var {:> H0} Empty)
;;
{:> A D0} ;; type Expr = (Lit Integer)
{:> D0}   ;; type Expr = (Lit Datum)
{:> E0}   ;;           | (Eof)
{:> H0}   ;;           | (Empty)
{:> E0}   ;;           | (Prim0 Op0)
{:> B}    ;;           | (Prim1 Op1 Expr)
{:> C D0} ;;           | (IfZero Expr Expr Expr)
{:> D0}   ;;           | (If Expr Expr Expr)
{:> F}    ;;           | (Let Id Expr Expr)
{:> F}    ;;           | (Var Id)
{:> F}    ;; type Id  = Symbol
{:> D0}   ;; type Datum = Integer
{:> D0}   ;;            | Boolean
{:> D1}   ;;            | Character
{:> E0}   ;; type Op0 = 'read-byte | 'peek-byte | 'void
{:> B}    ;; type Op1 = 'add1 | 'sub1
{:> D0}   ;;          | 'zero?
{:> D1}   ;;          | 'char? | 'integer->char | 'char->integer
{:> E0}   ;;          | 'write-byte | 'eof-object?
{:> H0}   ;;          | 'box | 'car | 'cdr | 'unbox
{:> H0}   ;;          | 'empty? | 'cons? | 'box?
{:> F}    ;; type Op2 = '+ | '- | '< | '=
{:> H0}   ;;          | eq? | 'cons

{:> E0}   (struct Eof () #:prefab)
{:> H0}   (struct Empty () #:prefab)
{:> A D0} (struct Lit (i) #:prefab)
{:> D0}   (struct Lit (d) #:prefab)
{:> E0}   (struct Prim0 (p) #:prefab)
{:> B}    (struct Prim1 (p e) #:prefab)
{:> F}    (struct Prim2 (p e1 e2)  #:prefab)
{:> C D0} (struct IfZero (e1 e2 e3) #:prefab)
{:> D0}   (struct If (e1 e2 e3) #:prefab)
{:> E0}   (struct Begin (e1 e2) #:prefab)
{:> F}    (struct Let (x e1 e2) #:prefab)
{:> F}    (struct Var (x) #:prefab)
