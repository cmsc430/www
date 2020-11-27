#lang racket
(provide Int Bool If Prim1 Prim2 Let Var Empty Prog Defn App)

;; type Prog = (Prog (Listof Defn) Expr)
(struct Prog (ds e) #:prefab)

;; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f xs e) #:prefab)

;; type Expr =
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Prim1 Op1 Expr)
;; | (Prim2 Op2 Expr Expr)
;; | (If Expr Expr Expr)
;; | (Let Id Expr Expr)
;; | (Var Id)
;; | (App Id (Listof Expr))
;; type Id = Symbol
;; type Op1 = 'add1 | 'sub1 | 'zero? | 'box | 'unbox | 'car | 'cdr
;; type Op2 = '+ | '- | 'cons
(struct Int (i) #:prefab)
(struct Bool (b) #:prefab)
(struct Prim1 (p e) #:prefab)
(struct Prim2 (p e1 e2) #:prefab)
(struct If (e1 e2 e3) #:prefab)
(struct Let (x e1 e2) #:prefab)
(struct Var (x) #:prefab)
(struct Empty () #:prefab)
(struct App (f es) #:prefab)
