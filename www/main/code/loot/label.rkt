#lang racket
(require "ast.rkt")
(provide (all-defined-out))

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
    [(Lam n xs e)    (Lam (gensym 'lam) xs (label-λ e))]
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
(define (λs e)
  (match e
    [(Prog ds e)     (append (append-map λs ds) (λs e))]
    [(Defn f xs e)   (λs e)]
    [(Prim1 p e)     (λs e)]
    [(Prim2 p e1 e2) (append (λs e1) (λs e2))]
    [(If e1 e2 e3)   (append (λs e1) (λs e2) (λs e3))]
    [(Begin e1 e2)   (append (λs e1) (λs e2))]
    [(Let x e1 e2)   (append (λs e1) (λs e2))]
    [(LetRec bs e1)  (append (append-map (lambda (xs) ((compose cdr map) λs xs)) bs) (λs e1))]
    [(Lam n xs e1)   (cons e (λs e1))]
    [(App f es)      (append (λs f) (append-map λs es))]
    [_               '()]))

;; For those that struggle with typing unicode
(define lambdas λs)
