#lang racket
(require "ast.rkt")
(provide lambdas lambdas-ds)

;; Prog -> [Listof Lam]
;; List all of the lambda expressions in p
(define (lambdas p)
  (match p
    [(Prog ds)
     (lambdas-ds ds)]))

;; Defns -> [Listof Lam]
;; List all of the lambda expressions in ds
(define (lambdas-ds ds)
  (match ds
    ['() '()]
    [(cons (Defn f l) ds)
     (append (lambdas-e l)
             (lambdas-ds ds))]))

;; Expr -> [Listof Lam]
;; List all of the lambda expressions in e
(define (lambdas-e e)
  (match e
    [(Prim p es)        (append-map lambdas-e es)]
    [(If e1 e2 e3)      (append (lambdas-e e1) (lambdas-e e2) (lambdas-e e3))]
    [(Begin es)         (append-map lambdas-e es)]
    [(Let xs es e)      (append (append-map lambdas-e es) (lambdas-e e))]
    [(App e1 es)        (append (lambdas-e e1) (append-map lambdas-e es))]
    [(Lam f xs e1)       (cons e (lambdas-e e1))]
    [(LamRest f xs x e1) (cons e (lambdas-e e1))]
    [(LamCase f cs)      (cons e (lambdas-cs cs))]
    [(Apply e es el)     (append (lambdas-e e) (append-map lambdas-e es) (lambdas-e el))]
    [(Match e ps es)    (append (lambdas-e e)
                                (append-map lambdas-pat ps)
                                (append-map lambdas-e es))]
    [_                  '()]))

;; [Listof LamCaseClause] -> [Listof Lam]
(define (lambdas-cs cs)
  (match cs
    ['() '()]
    [(cons (Lam f xs e) cs)
     (append (lambdas-e e)
             (lambdas-cs cs))]
    [(cons (LamRest f xs x e) cs)
     (append (lambdas-e e)
             (lambdas-cs cs))]))

;; Pat -> [Listof Lam]
(define (lambdas-pat p)
  (match p
    [(PBox p) (lambdas-pat p)]
    [(PCons p1 p2) (append (lambdas-pat p1) (lambdas-pat p2))]
    [(PAnd p1 p2) (append (lambdas-pat p1) (lambdas-pat p2))]
    [(PStruct n ps) (append-map lambdas-pat ps)]
    [(PPred e) (lambdas-e e)]
    [_ '()]))
