#lang racket
(provide (all-defined-out))

(require "ast.rkt")

; In order to desugar a program into a single let-rec, we take all of the
; top-level definitions and convert them into bindings for a top-level
; let-rec
(define (desugar-prog p)
  (match p
    [(prog ds e) (let ((bs (map desugar-def ds)))
                      (prog '() (letr-e bs e)))]))

(define (desugar-def d)
  (match d
    [(fundef n args body)
      (binding n (lam-e args body))]))

;; Expr+ -> Expr
; The only case that is interesting is the `letr-e` case, where bindings
; get turned into lambdas
(define (desugar e+)
  (match e+
    [(? imm? i)       e+]
    [(var-e v)        e+]
    [(prim-e p es)    (prim-e p (map desugar es))]
    [(if-e e0 e1 e2)  (if-e (desugar e0) (desugar e1) (desugar e2))]
    [(let-e bs body)  (let-e (bindings-map-def desugar bs) (desugar body))]
    [(letr-e bs body) (letr-e (bindings-map-def desugar bs) (desugar body))]
    [(lam-e xs e0)    (lam-e xs (desugar e0))]
    [(app-e f es)     (app-e (desugar f) (map desugar es))]))

;; Any -> Boolean
(define (imm? x)
  (or (int-e? x)
      (bool-e? x)
      (char-e? x)
      (nil-e? x)))

;; Expr -> LExpr
(define (label-λ e)
  (match e
    [(? imm? i)       e]
    [(var-e v)        e]
    [(prim-e p es)    (prim-e p (map label-λ es))]
    [(if-e e0 e1 e2)  (if-e (label-λ e0) (label-λ e1) (label-λ e2))]
    [(let-e bs body)  (let-e (bindings-map-def label-λ bs) (label-λ body))]
    [(letr-e bs body) (letr-e (bindings-map-def label-λ bs) (label-λ body))]
    [(lam-e xs e0)    (lam-t (gensym) xs (label-λ e0))]
    [(app-e f es)     (app-e (label-λ f) (map label-λ es))]))

;; LExpr -> (Listof LExpr)
;; Extract all the lambda expressions
(define (λs e)
  (match e
    [(? imm? i)       '()]
    [(var-e v)        '()]
    [(prim-e p es)    (apply append (map λs es))]
    [(if-e e0 e1 e2)  (append (λs e0) (λs e1) (λs e2))]
    [(let-e (list (binding v def)) body)
                      (append (λs def) (λs body))]
    [(letr-e bs body) (append (apply append (map λs (get-defs bs))) (λs body))]
    [(lam-e xs e0)    (cons e (λs e0))]
    [(lam-t _ xs e0)  (cons e (λs e0))]
    [(app-e f es)     (append (λs f) (apply append (map λs es)))]))

;; LExpr -> (Listof Variable)
(define (fvs e)
  (define (fvs e)
    (match e
      [(? imm? i)       '()]
      [(var-e v)        (list v)]
      [(prim-e p es)    (apply append (map fvs es))]
      [(if-e e0 e1 e2)  (append (fvs e0) (fvs e1) (fvs e2))]
      [(let-e bs body)  (append (apply append (map fvs (get-defs bs)))
                                (remq* (get-vars bs) (fvs body)))]
      [(letr-e bs body) (remq* (get-vars bs) (append (apply append (map fvs (get-defs bs))) (fvs body)))]
      [(lam-t _ xs e0)  (remq* xs (fvs e0))]
      [(lam-e xs e0)    (remq* xs (fvs e0))]
      [(app-e f es)     (append (fvs f) (apply append (map fvs es)))]))
  (remove-duplicates (fvs e)))

; SExpr -> Prog
(define (sexpr->prog s)
  (match s
    [(list 'begin defs ... e) (prog (map sexpr->fundef defs) (sexpr->expr e))]
    [e                        (prog '() (sexpr->expr e))]))

; SExpr -> FunDef
(define (sexpr->fundef def)
  (match def
    [`(define (,f . ,as) ,body) (fundef f as (sexpr->expr body))]))

; SExpr -> Expr
; Parse the s-expr into our Expr AST
; This should be a one-to-one mapping for now.
(define (sexpr->expr s)
  (match s
    [(? symbol? v)   (var-e v)]
    [(? integer? s)  (int-e s)]
    [(? boolean? b)  (bool-e b)]
    [(? char? c)     (char-e c)]
    [''()            (nil-e)]
    [`(if ,p ,t ,f)  (if-e (sexpr->expr p) (sexpr->expr t) (sexpr->expr f))]
    [`(let ((,bnd ,def)) ,body)
                     (let-e (list (binding bnd (sexpr->expr def))) (sexpr->expr body))]
    [`(letrec ,bs ,body)
                     (letr-e (map (lambda (b) (binding (first b) (sexpr->expr (second b)))) bs) (sexpr->expr body))]
    [`(,(? unop? p) ,e)
                     (prim-e p (list (sexpr->expr e)))]
    [`(,(? biop? p) ,e1 ,e2)
                     (prim-e p (list (sexpr->expr e1) (sexpr->expr e2)))]
    [`(λ ,xs ,e0)    (lam-e xs (sexpr->expr e0))]
    [`(lambda ,a ,e) (lam-e a (sexpr->expr e))]
    [`(,f . ,as)     (app-e (sexpr->expr f) (map sexpr->expr as))]
    [_               (error "operation not supported")]))
