#lang crook
{:= A B C D0 D0.A D1 E0 E1 F F.A H0 H1 I J K L}
(provide {:> A} Lit {:> E0} Prim0 {:> B} Prim1 {:> F} Prim2 {:> H1} Prim3
         {:> C D0} IfZero {:> D0} If {:> E0} Eof {:> E0} Begin
         {:> F} Let {:> F} Var {:> I} Prog {:> I} Defn {:> I} App
         {:> F.A H} Cond {:> F.A H} Case
         {:> K} Match {:> K} Box {:> K} Cons {:> K} Conj {:> L} Lam)

{:> I} ;; type Prog = (Prog (Listof Defn) Expr)
{:> I} (struct Prog (ds e) #:prefab)

{:> I} ;; type Defn = (Defn Id (Listof Id) Expr)
{:> I} (struct Defn (f xs e) #:prefab)

{:> A D0} ;; type Expr = (Lit Integer)
{:> D0}   ;; type Expr = (Lit Datum)
{:> E0}   ;;           | (Eof)
{:> E0}   ;;           | (Prim0 Op0)
{:> B}    ;;           | (Prim1 Op1 Expr)
{:> F}    ;;           | (Prim2 Op2 Expr Expr)
{:> H1}   ;;           | (Prim3 Op3 Expr Expr Expr)
{:> C D0} ;;           | (IfZero Expr Expr Expr)
{:> D0}   ;;           | (If Expr Expr Expr)
{:> D0.A D1}
          ;;           | (Cond [Listof CondClause] Expr)
{:> F.A H0}
          ;;           | (Cond [Listof CondClause] Expr)
{:> D0.A D1}
          ;;           | (Case Expr [Listof CaseClause] Expr)
{:> F.A H0}
          ;;           | (Case Expr [Listof CaseClause] Expr)
{:> E0}   ;;           | (Begin Expr Expr)
{:> F F.A}
          ;;           | (Let Id Expr Expr)
{:> F.A H0}
          ;;           | (Let (Listof Id) (Listof Expr) Expr)
{:> H0}   ;;           | (Let Id Expr Expr)
{:> F}    ;;           | (Var Id)
{:> I L}  ;;           | (App Id (Listof Expr))
{:> L}    ;;           | (App Expr (Listof Expr))
{:> K}    ;;           | (Match Expr (Listof Pat) (Listof Expr))
{:> L}    ;;           | (Lam Id (Listof Id) Expr)

{:> D0.A D1}
;; type CondClause = (Clause Expr Expr)
{:> D0.A D1}
;; type CaseClause = (Clause [Listof Datum] Expr)
{:> F.A H0}
;; type CondClause = (Clause Expr Expr)
{:> F.A H0}
;; type CaseClause = (Clause [Listof Datum] Expr)

{:> F}    ;; type Id  = Symbol
{:> D0}   ;; type Datum = Integer
{:> D0}   ;;            | Boolean
{:> D1}   ;;            | Character
{:> H1}   ;;            | String
{:> E0}   ;; type Op0 = 'read-byte | 'peek-byte | 'void
{:> B}    ;; type Op1 = 'add1 | 'sub1
{:> D0}   ;;          | 'zero?
{:> D0.A D1}
          ;;          | 'abs | '- | 'not
{:> D1}   ;;          | 'char? | 'integer->char | 'char->integer
{:> E0}   ;;          | 'write-byte | 'eof-object?
{:> H0}   ;;          | 'box | 'car | 'cdr | 'unbox
{:> H0}   ;;          | 'empty? | 'cons? | 'box?
{:> H1}   ;;          | 'vector? | 'vector-length
{:> H1}   ;;          | 'string? | 'string-length
{:> F}    ;; type Op2 = '+ | '- | '< | '=
{:> H0}   ;;          | 'eq? | 'cons
{:> H1}   ;;          | 'make-vector | 'vector-ref
{:> H1}   ;;          | 'make-string | 'string-ref
{:> H1}   ;; type Op3 = 'vector-set!
{:> K}    ;; type Pat  = (Var Id)
{:> K}    ;;           | (Lit Datum)
{:> K}    ;;           | (Box Pat)
{:> K}    ;;           | (Cons Pat Pat)
{:> K}    ;;           | (Conj Pat Pat)

{:> E0}   (struct Eof () #:prefab)
{:> A D0} (struct Lit (i) #:prefab)
{:> D0}   (struct Lit (d) #:prefab)
{:> E0}   (struct Prim0 (p) #:prefab)
{:> B}    (struct Prim1 (p e) #:prefab)
{:> F}    (struct Prim2 (p e1 e2)  #:prefab)
{:> H1}   (struct Prim3 (p e1 e2 e3)  #:prefab)
{:> C D0} (struct IfZero (e1 e2 e3) #:prefab)
{:> D0}   (struct If (e1 e2 e3) #:prefab)
{:> E0}   (struct Begin (e1 e2) #:prefab)
{:> F}    (struct Let (x e1 e2) #:prefab)
{:> F.A H} (struct Cond (cs es el) #:prefab)
{:> F.A H} (struct Case (e ds es el) #:prefab)
{:> F}    (struct Var (x) #:prefab)
{:> I}    (struct App (f es) #:prefab)
{:> L}    (struct Lam (f xs e) #:prefab)
{:> K}    (struct Match (e ps es) #:prefab)

{:> K}    (struct Box (p) #:prefab)
{:> K}    (struct Cons (p1 p2) #:prefab)
{:> K}    (struct Conj (p1 p2) #:prefab)
