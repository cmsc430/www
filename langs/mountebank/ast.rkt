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
;;            | (Match Expr (Listof Pat) (Listof Expr))
;;            | (App Expr (Listof Expr))
;;            | (Lam Id (Listof Id) Expr)
;; type Datum = Integer
;;            | Char
;;            | Boolean
;;            | String
;;            | Symbol
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
;;            | 'vector? | 'vector-length
;;            | 'string? | 'string-length
;;            | 'symbol? | 'string->symbol
;;            | 'string->symbol | 'string->uninterned-symbol
;; type Op2   = '+ | '- | '< | '=
;;            | 'cons | 'eq?
;;            | 'make-vector | 'vector-ref
;;            | 'make-string | 'string-ref
;; type Op3   = 'vector-set!
;; type Pat  = (PVar Id)
;;           | (PWild)
;;           | (PLit Lit)
;;           | (PBox Pat)
;;           | (PCons Pat Pat)
;;           | (PAnd Pat Pat)
;;           | (PSymb Symbol)
;;           | (PStr String)
;; type Lit  = Boolean
;;           | Character
;;           | Integer
;;           | '()

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
(struct Match (e ps es)    #:prefab)

(struct PVar  (x)          #:prefab)
(struct PWild ()           #:prefab)
(struct PLit  (x)          #:prefab)
(struct PBox  (p)          #:prefab)
(struct PCons (p1 p2)      #:prefab)
(struct PAnd  (p1 p2)      #:prefab)
(struct PSymb (s)          #:prefab)
(struct PStr (s)           #:prefab)
