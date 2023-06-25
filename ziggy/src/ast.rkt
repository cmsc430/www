#lang crook
{:= A B C D0 D1 E0 E1}
(provide {:> A} Lit {:> E0} Prim0 {:> B} Prim1 {:> C D0} IfZero {:> D0} If {:> E0} Eof {:> E0} Begin)
;;
{:> A D0} ;; type Expr = (Lit Integer)
{:> D0}   ;; type Expr = (Lit Datum)
{:> E0}   ;;           | (Eof)
{:> E0}   ;;           | (Prim0 Op0)
{:> B}    ;;           | (Prim1 Op1 Expr)
{:> C D0} ;;           | (IfZero Expr Expr Expr)
{:> D0}   ;;           | (If Expr Expr Expr)
{:> D0}   ;; type Datum = Integer
{:> D0}   ;;            | Boolean
{:> D1}   ;;            | Character
{:> E0}   ;; type Op0 = 'read-byte | 'peek-byte | 'void
{:> B}    ;; type Op1 = 'add1 | 'sub1
{:> D0}   ;;          | 'zero?
{:> D1}   ;;          | 'char? | 'integer->char | 'char->integer
{:> E0}   ;;          | 'write-byte | 'eof-object?

{:> E0}   (struct Eof () #:prefab)
{:> A D0} (struct Lit (i) #:prefab)
{:> D0}   (struct Lit (d) #:prefab)
{:> E0}   (struct Prim0 (p) #:prefab)
{:> B}    (struct Prim1 (p e) #:prefab)
{:> C D0} (struct IfZero (e1 e2 e3) #:prefab)
{:> D0}   (struct If (e1 e2 e3) #:prefab)
{:> E0}   (struct Begin (e1 e2) #:prefab)
