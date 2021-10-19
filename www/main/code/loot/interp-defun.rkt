#lang racket
(provide interp interp-env interp-prim1 apply-function)
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
;; | Function

;; type Function =
;; | `(closure ,Formals ,Expr ,Env)
;; | `(rec-closure ,Lambda ,(-> Env))

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
                          (map (λ (l) (RecClosure l r*))
                               (map cadr bs)))
                     r))))
       (interp-env e (r*)))]
    [(Lam _ xs e1)
      (Closure xs e r)]
    [(App f es)
     (match (interp-env* (cons f es) r)       
       [(list (? function? f) vs ...)
        (apply apply-function f vs)]        
       [e e])]
    [_         'err]))

(define (function? f)
  (match f
    [(Closure _ _ _) #t]
    [(RecClosure _ _) #t]
    [(? procedure?)  #t]
    [_ #f]))

;; Function Value ... -> Answer
(define (apply-function f . vs)
  (match f
    [(Closure xs e r)
     (if (= (length xs) (length vs))
         (interp-env e (append (zip xs vs) r))
         'errwat)]
    [(RecClosure (Lam '() xs e) r*)
      ; You've got to apply the the r* thunk
     (apply apply-function (Closure xs e (r*)) vs)]
    [(? procedure? f) (apply f vs)]))


;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (interp-env* es r)
  (match es
    ['() '()]
    [(cons e es)
      (match (interp-env e r)
       ['err 'errsdf]
       [v (cons v (interp-env* es r))])]))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))
