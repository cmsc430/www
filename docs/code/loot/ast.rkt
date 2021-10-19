#lang racket
(provide (all-defined-out))

;; type Prog = (Prog (Listof Defn) Expr)
(struct Prog (ds e) #:prefab)

;; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f xs e) #:prefab)

;; Differences from Knock
;;
;;  * We _remove_:
;;    - `Fun`
;;    - `Call`
;;
;;  * We add
;;    - `Lam`
;;
;;  * We change:
;;    - `App`
;;
;; type Expr = (Eof)
;;           | (Empty)
;;           | (Int Integer)
;;           | (Bool Boolean)
;;           | (Char Character)
;;           | (Prim0 Op0)
;;           | (Prim1 Op1 Expr)
;;           | (Prim1 Op2 Op2 Expr)
;;           | (If Expr Expr Expr)
;;           | (Begin Expr Expr)
;;           | (Let Id Expr Expr)
;;           | LetRec (Binding list) Expr <--- New for Loot (See the lecture notes!)
;;           | Lam Name [Variable] Expr   <--- New for Loot
;;           | (Var Id)
;;           | (App Expr (Listof Expr))   <--- Changed from Knock
;; type Id   = Symbol
;; type Op0  = 'read-byte | 'void | 'collect-garbage
;; type Op1  = 'add1 | 'sub1 | 'zero?
;;           | 'char? | 'integer->char | 'char->integer
;;           | 'write-byte | 'eof-object?
;;           | 'box | 'car | 'cdr | 'unbox
;;           | 'empty?
;; type Op2  = '+ | '- | 'eq?
;;           | 'cons
(struct Eof    ()           #:prefab)
(struct Empty  ()           #:prefab)
(struct Int    (i)          #:prefab)
(struct Bool   (b)          #:prefab)
(struct Char   (c)          #:prefab)
(struct Prim0  (p)          #:prefab)
(struct Prim1  (p e)        #:prefab)
(struct Prim2  (p e1 e2)    #:prefab)
(struct If     (e1 e2 e3)   #:prefab)
(struct Begin  (e1 e2)      #:prefab)
(struct Let    (x e1 e2)    #:prefab)
(struct LetRec (bs e1)      #:prefab)
(struct Lam    (n xs e)     #:prefab)
(struct Var    (x)          #:prefab)
(struct App    (f es)       #:prefab)


;; For pedagogical purposes
(struct Closure (xs e r)  #:prefab)
(struct RecClosure (f r)  #:prefab)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Free Variables
;;
;; Expr -> [Var]
(define (fvs e)
  (define (fvs e)
    (match e
      [(Prim1 p e)     (fvs e)]
      [(Prim2 p e1 e2) (append (fvs e1) (fvs e2))]
      [(If e1 e2 e3)   (append (fvs e1) (fvs e2) (fvs e3))]
      [(Begin e1 e2)   (append (fvs e1) (fvs e2))]
      [(Let x e1 e2)   (append (fvs e1) (remq* (list x) (fvs e2)))]
      [(LetRec bs e1)  (let ((bound (map car bs))
                             (def-fvs (append-map fvs-bind bs)))
                            (remq* bound (append def-fvs (fvs e1))))]
      [(Lam n xs e1)   (remq* xs (fvs e1))]
      [(Var x)         (list x)]
      [(App f es)      (append (fvs f) (append-map fvs es))]
      [_               '()]))
  (remove-duplicates (fvs e)))





(define (fvs-bind d)
  (match d
    [(list x e1) (fvs e1)]))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desugaring Definitions
;;
;; Now that we have lambdas, we can actually treat user-defined functions
;; as syntactic sugar for lambdas. For example:
;;
;; (begin
;;   (define (f x) (+ x x))
;;   (f 42))
;;
;; Can be transformed to:
;;
;; (let ((f (lambda (x) (+ x x))))
;;      (f 42))
;;
;; That's not _quite_ enough, as top-level functions can refer to each other:
;;
;; (begin
;;   (define (f x) (+ x x))
;;   (define (g y) (+ (f y) y))
;;   (g 42))
;;
;; Becomes:
;;
;; (letrec ((f (lambda (x) (+ x x)))
;;          (g (lambda (y) (+ (f y) y))))
;;         (g 42))
;;
;; Since we can represent our programs using this 'more fundamental' feature
;; we can always _desugar_ from the nice-to-write version to the more
;; fundamental version.
;;
;; Prog -> Prog
(define (desugar e+)
  (match e+
    [(Prog '() e)    (Prog '() (desugar e))]
    [(Prog ds e)     (let ((defs (map desugar ds)))
                          (Prog '() (LetRec defs e)))]
    [(Defn f xs e)   (list f (Lam f xs e))]
    [(Prim1 p e)     (Prim1 p (desugar e))]
    [(Prim2 p e1 e2) (Prim2 p (desugar e1) (desugar e2))]
    [(If e1 e2 e3)   (If (desugar e1) (desugar e2) (desugar e3))]
    [(Begin e1 e2)   (Begin (desugar e1) (desugar e2))]
    [(Let x e1 e2)   (Let x (desugar e1) (desugar e2))]
    [(LetRec bs e1)  (LetRec (map (lambda (xs) (map desugar xs)) bs) (desugar e1))]
    [(Lam n xs e)    (Lam (gensym 'lam) xs (desugar e))]
    [(App f es)      (App (desugar f) (map desugar es))]
    [_               e+]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Labelling Lambdas
;;
;; Each lambda in a program needs to have a unique name so that we know what
;; code we need to jump to when that lambda is 'called'.
;; Luckily, `gensym` provides all the functionality that we need here.
;;
;; The flat values are easy: no possibility of there being a lambda, so
;; we just return the unaltered expression. For everything else we traverse
;; down the structure, the only case that actually 'does' anything is
;; for `Lam`
;;
;; Prog -> Prog
(define (label-λ e)
  (match e
    [(Prog ds e)     (Prog (map label-λ ds) (label-λ e))]
    [(Defn f xs e)   (Defn f xs (label-λ e))]
    [(Prim1 p e)     (Prim1 p (label-λ e))]
    [(Prim2 p e1 e2) (Prim2 p (label-λ e1) (label-λ e2))]
    [(If e1 e2 e3)   (If (label-λ e1) (label-λ e2) (label-λ e3))]
    [(Begin e1 e2)   (Begin (label-λ e1) (label-λ e2))]
    [(Let x e1 e2)   (Let x (label-λ e1) (label-λ e2))]
    [(LetRec bs e1)  (LetRec (map (lambda (xs) (map label-λ xs)) bs) (label-λ e1))]
    [(Lam '() xs e)  (Lam (gensym 'lam) xs (label-λ e))]
    [(Lam n xs e)    (Lam (gensym n) xs (label-λ e))]
    [(App f es)      (App (label-λ f) (map label-λ es))]
    [_               e]))

;; For those that struggle with typing unicode
(define label-lambda label-λ)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting all Lambdas
;;
;; While the lambdas could be _written_ anywhere in the source code, we do need
;; to write the generated target code somewhere reliable. There are a few ways
;; to do this, but we've decided to take the most straightforward route: collect
;; the lambdas and treat them as 'additional' function definitions.
;;
;; In order to do this we'll need a list of all the lambdas in a program.
;; This function traverses our program and collects all the lambdas.
;;
;; Prog -> [Expr]
(define (λs e)
  (match e
    [(Prog ds e)     (append (append-map λs ds) (λs e))]
    [(Defn f xs e)   (λs e)]
    [(Prim1 p e)     (λs e)]
    [(Prim2 p e1 e2) (append (λs e1) (λs e2))]
    [(If e1 e2 e3)   (append (λs e1) (λs e2) (λs e3))]
    [(Begin e1 e2)   (append (λs e1) (λs e2))]
    [(Let x e1 e2)   (append (λs e1) (λs e2))]
    [(LetRec bs e1)  (append (append-map lambda-defs bs) (λs e1))]
    [(Lam n xs e1)   (cons e (λs e1))]
    [(App f es)      (append (λs f) (append-map λs es))]
    [_               '()]))

(define (lambda-defs d)
  (match d
    [(list x e) (λs e)]))

;; For those that struggle with typing unicode
(define lambdas λs)
