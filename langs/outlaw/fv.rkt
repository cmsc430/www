#lang racket
(require "ast.rkt")
(provide fv fv-)

;; Expr -> [Listof Id]
;; List all of the free variables in e
(define (fv e)
  (remove-duplicates (fv* e) eq?))

;; Expr [Listof Id] -> [Listof Id]
(define (fv- e xs)
  (remq* xs (fv e)))

(define (fv* e)  
  (match e
    [(Var x)            (list x)]
    [(Prim p es)        (append-map fv* es)]
    [(If e1 e2 e3)      (append (fv* e1) (fv* e2) (fv* e3))]
    [(Begin es)         (append-map fv* es)]
    [(Let xs es e)      (append (append-map fv* es) (remq* xs (fv* e)))]    
    [(App e1 es)        (append (fv* e1) (append-map fv* es))]
    [(Lam f xs e)       (remq* xs (fv* e))]
    [(LamRest f xs x e) (remq* (cons x xs) (fv* e))]
    [(LamCase f cs)     (append-map fv* cs)]
    [(Apply e es el)    (append (fv* e) (append-map fv* es) (fv* el))]
    [(Match e ps es)    (append (fv* e) (append-map fv-clause* ps es))]
    [_                  '()]))

;; Pat Expr -> [Listof Id]
(define (fv-clause* p e)
  (remq* (bv-pat* p) (append (fv-pat* e) (fv* e))))

;; Pat -> [Listof Id]
(define (bv-pat* p)
  (match p
    [(PVar x) (list x)]
    [(PCons p1 p2) (append (bv-pat* p1) (bv-pat* p2))]
    [(PAnd p1 p2) (append (bv-pat* p1) (bv-pat* p2))]
    [(PBox p) (bv-pat* p)]
    [(PStruct n ps) (append-map bv-pat* ps)]
    [_ '()]))

;; Pat -> [Listof Id]
(define (fv-pat* p)
  (match p
    [(PBox p) (fv-pat* p)]
    [(PCons p1 p2) (append (fv-pat* p1) (fv-pat* p2))]
    [(PAnd p1 p2) (append (fv-pat* p1) (fv-pat* p2))]
    [(PStruct n ps) (append-map fv-pat* ps)]
    [(PPred e) (fv* e)]
    [_ '()]))
  
