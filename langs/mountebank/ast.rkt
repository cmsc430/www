#lang racket
(provide (all-defined-out))

;; type Prog = (Prog (Listof Defn) Expr)
(struct Prog (ds e) #:prefab)

;; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f xs e) #:prefab)

;; type Expr  = (Eof)
;;            | (Quote Datum)
;;            | (Prim0 Op0)
;;            | (Prim1 Op1 Expr)
;;            | (Prim2 Op2 Expr Expr)
;;            | (Prim3 Op3 Expr Expr Expr)
;;            | (If Expr Expr Expr)
;;            | (Begin Expr Expr)
;;            | (Let Id Expr Expr)
;;            | (Var Id)
;;            | (App Expr (Listof Expr))
;;            | (Lam Id (Listof Id) Expr)
;; type Datum = Integer
;;            | Char
;;            | Boolean
;;            | String
;;            | (Boxof Datum)
;;            | (Listof Datum)
;;            | (Vectorof Datum)
;; type Id    = Symbol
;; type Op0   = 'read-byte
;; type Op1   = 'add1 | 'sub1 | 'zero?
;;            | 'char? | 'integer->char | 'char->integer
;;            | 'write-byte | 'eof-object?
;;            | 'box | 'car | 'cdr | 'unbox
;;            | 'empty? | 'cons? | 'box?
;;            | 'vector? | vector-length
;;            | 'string? | string-length
;; type Op2   = '+ | '- | '< | '=
;;            | 'cons | 'eq?
;;            | 'make-vector | 'vector-ref
;;            | 'make-string | 'string-ref
;; type Op3   = 'vector-set!

(struct Eof   ()           #:prefab)
(struct Prim0 (p)          #:prefab)
(struct Prim1 (p e)        #:prefab)
(struct Prim2 (p e1 e2)    #:prefab)
(struct Prim3 (p e1 e2 e3) #:prefab)
(struct If    (e1 e2 e3)   #:prefab)
(struct Begin (e1 e2)      #:prefab)
(struct Let   (x e1 e2)    #:prefab)
(struct Var   (x)          #:prefab)
(struct App   (e es)       #:prefab)
(struct Lam   (f xs e)     #:prefab)
(struct Quote (d)          #:prefab)
