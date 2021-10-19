#lang racket
(provide interp interp-env interp-prim1)
(require "ast.rkt"
         "env.rkt"
         "interp-prims.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | (Fun f)
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)

;; type REnv = (Listof (List Id Value))
;; type Defns = (Listof Defn)

;; Prog Defns -> Answer
(define (interp p)
  (interp-env (desugar p) '()))

;; Expr Env Defns -> Answer
(define (interp-env e r)
  (match e
    [(Prog '() e) (interp-env e r)]
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof)    eof]
    [(Empty)  '()]
    [(Var x)  (lookup r x)]
    [(Prim0 'void) (void)]
    [(Prim0 'read-byte) (read-byte)]
    [(Prim0 'peek-byte) (peek-byte)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [_ (interp-env e2 r)])]
    [(Let x e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v (interp-env e2 (ext r x v))])]
    [(LetRec bs e)
     (letrec ((r* (λ ()
                    (append
                     (zip (map car bs)
                          ;; η-expansion to delay evaluating r*
                          ;; relies on RHSs being functions
                          (map (λ (l) (λ vs (apply (interp-env l (r*)) vs)))
                               (map cadr bs)))
                     r))))
       (interp-env e (r*)))]
    [(Lam _ xs e1)
        (lambda vs
          (if (= (length vs) (length xs))
              (interp-env e1 (append (zip xs vs) r))
              'err))]
    [(App f es)
     (match (interp-env* (cons f es) r)
      [(list f vs ...)
       (if (procedure? f)
           (apply f vs)
           'err)])]
    [_         'err]))

;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (interp-env* es r)
  (match es
    ['() '()]
    [(cons e es)
      (match (interp-env e r)
       ['err 'err]
       [v (cons v (interp-env* es r))])]))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))
