#lang racket
(provide (all-defined-out))

;; type Prog = (Prog (Listof Defn) Expr)
(struct Prog (ds e) #:prefab)

;; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f xs e) #:prefab)

;; type Expr = (Eof)
;;           | (Empty)
;;           | (Fun Id)                  <-- New for Knock
;;           | (Int Integer)
;;           | (Bool Boolean)
;;           | (Char Character)
;;           | (Prim0 Op0)
;;           | (Prim1 Op1 Expr)
;;           | (Prim1 Op2 Op2 Expr)
;;           | (If Expr Expr Expr)
;;           | (Begin Expr Expr)
;;           | (Let Id Expr Expr)
;;           | (Var Id)
;;           | (App Id (Listof Expr))
;;           | (Call Expr (Listof Expr)) <-- New for Knock
;; type Id   = Symbol
;; type Op0  = 'read-byte | 'void | 'collect-garbage
;; type Op1  = 'add1 | 'sub1 | 'zero?
;;           | 'char? | 'integer->char | 'char->integer
;;           | 'write-byte | 'eof-object?
;;           | 'box | 'car | 'cdr | 'unbox
;;           | 'empty?
;; type Op2  = '+ | '- | 'eq?
;;           | 'cons
(struct Eof   ()           #:prefab)
(struct Empty ()           #:prefab)
(struct Int   (i)          #:prefab)
(struct Bool  (b)          #:prefab)
(struct Char  (c)          #:prefab)
(struct Prim0 (p)          #:prefab)
(struct Prim1 (p e)        #:prefab)
(struct Prim2 (p e1 e2)    #:prefab)
(struct If    (e1 e2 e3)   #:prefab)
(struct Begin (e1 e2)      #:prefab)
(struct Let   (x e1 e2)    #:prefab)
(struct Var   (x)          #:prefab)
(struct Fun   (f)          #:prefab)
(struct App   (f es)       #:prefab)
(struct FCall (f es)       #:prefab)


;; Helper functions

;; Does an Expr represent an immediate (i.e. flat) value?
;; Expr -> Bool
(define (imm? e)
  (match e
    [(Int i)  #t]
    [(Bool b) #t]
    [(Char c) #t]
    [(Eof)    #t]
    [(Empty)  #t]
    [_        #f]))

;; Get the 'actual' value out of an immediate.
;; Expr -> Imm
(define (get-imm e)
  (match e
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof)    eof]
    [(Empty)  '()]
    [_        (error (~a "get-imm: " e " is not an immedate!"))]))
