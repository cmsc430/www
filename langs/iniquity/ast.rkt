#lang racket
(provide Lit Prim0 Prim1 Prim2 Prim3 If Eof Begin
         Let Var Prog Defn App)

;; type Prog = (Prog (Listof Defn) Expr)
(struct Prog (ds e) #:prefab)

;; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f xs e) #:prefab)
;; type Expr = (Lit Datum)
;;           | (Eof)
;;           | (Prim0 Op0)
;;           | (Prim1 Op1 Expr)
;;           | (Prim2 Op2 Expr Expr)
;;           | (Prim3 Op3 Expr Expr Expr)
;;           | (If Expr Expr Expr)
;;           | (Begin Expr Expr)
;;           | (Let Id Expr Expr)
;;           | (Var Id)
;;           | (App Id (Listof Expr))

;; type Id  = Symbol
;; type Datum = Integer
;;            | Boolean
;;            | Character
;;            | String
;; type Op0 = 'read-byte | 'peek-byte | 'void
;; type Op1 = 'add1 | 'sub1
;;          | 'zero?
;;          | 'char? | 'integer->char | 'char->integer
;;          | 'write-byte | 'eof-object?
;;          | 'box | 'car | 'cdr | 'unbox
;;          | 'empty? | 'cons? | 'box?
;;          | 'vector? | 'vector-length
;;          | 'string? | 'string-length
;; type Op2 = '+ | '- | '< | '=
;;          | 'eq? | 'cons
;;          | 'make-vector | 'vector-ref
;;          | 'make-string | 'string-ref
;; type Op3 = 'vector-set!

(struct Eof () #:prefab)
(struct Lit (d) #:prefab)
(struct Prim0 (p) #:prefab)
(struct Prim1 (p e) #:prefab)
(struct Prim2 (p e1 e2)  #:prefab)
(struct Prim3 (p e1 e2 e3)  #:prefab)
(struct If (e1 e2 e3) #:prefab)
(struct Begin (e1 e2) #:prefab)
(struct Let (x e1 e2) #:prefab)
(struct Var (x) #:prefab)
(struct App (f es) #:prefab)

