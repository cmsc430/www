#lang racket
(require "ast.rkt" "types.rkt")
(provide intern
         (struct-out Ref))

;; type QEnv = [Listof (cons (U String Symbol) Ref)]

(struct Ref (label type-tag) #:prefab)

;; Datum QEnv -> (cons Datum QEnv)
;; Intern all literal strings and symbols.
;; Replaces occurrences of string and symbol literals with
;; a reference bound in qenv.
(define (intern-datum d q)
  (cond
    [(string? d) (intern! d q type-str)]
    [(symbol? d) (intern! d q type-symb)]
    [(box? d)
     (match (intern-datum (unbox d) q)
       [(cons d q)
        (cons (box d) q)])]
    [(cons? d)
     (match (intern-datums (list (car d) (cdr d)) q)
       [(cons (list d1 d2) q)
        (cons (cons d1 d2) q)])]
    [(vector? d)
     (match (intern-datums (vector->list d) q)
       [(cons ds q)
        (cons (apply vector ds) q)])]
    [else
     (cons d q)]))

;; (U String Symbol) QEnv -> [Maybe Ref]
(define (lookup d q)
  (match q
    ['() #f]
    [(cons (cons d0 r) q)
     (if (equal? d d0)
         r
         (lookup d q))]))

;; (U Symbol String) QEnv Tag -> QEnv
;; Either lookup prior reference or create a new one
(define (intern! s q type)
  (match (lookup s q)
    [#f
     (let ((l (gensym 'lit)))
       (let ((r (Ref l type)))
         (cons r
               (cons (cons s r) q))))]
    [r (cons r q)]))

;; Prog -> (cons Prog QEnv)
(define (intern p)
  (intern-prog p '()))

;; [Listof Datum] QEnv -> (cons [Listof Datum] QEnv)
(define (intern-datums ds q)
  (match ds
    ['() (cons ds q)]
    [(cons d ds)
     (match (intern-datum d q)
       [(cons d q)
        (match (intern-datums ds q)
          [(cons ds q)
           (cons (cons d ds) q)])])]))

;; Prog QEnv -> (cons Prog QEnv)
(define (intern-prog p q)
  (match p
    [(Prog ds e)
     (match (intern-ds ds q)
       [(cons ds q)
        (match (intern-e e q)
          [(cons e q)
           (cons (Prog ds e) q)])])]))

;; Defns QEnv -> (cons Defns QEnv)
(define (intern-ds ds q)
  (match ds
    ['() (cons '() q)]
    [(cons d ds)
     (match (intern-d d q)
       [(cons d q)
        (match (intern-ds ds q)
          [(cons ds q)
           (cons (cons d ds) q)])])]))

;; Defn QEnv -> (cons Defn QEnv)
(define (intern-d d q)
  (match d
    [(Defn f xs e)
     (match (intern-e e q)
       [(cons e q)
        (cons (Defn f xs e) q)])]))

;; Expr QEnv -> (cons Expr QEnv)
(define (intern-e e q)
  (match e
    [(Quote d)
     (match (intern-datum d q)
       [(cons d q)
        (cons (Quote d) q)])]
    [(Prim1 p e)
     (match (intern-e e q)
       [(cons e q)
        (cons (Prim1 p e) q)])]
    [(Prim2 p e1 e2)
     (match (intern-es (list e1 e2) q)
       [(cons (list e1 e2) q)
        (cons (Prim2 p e1 e2) q)])]
    [(Prim3 p e1 e2 e3)
     (match (intern-es (list e1 e2 e3) q)
       [(cons (list e1 e2 e3) q)
        (cons (Prim3 p e1 e2 e3) q)])]
    [(If e1 e2 e3)
     (match (intern-es (list e1 e2 e3) q)
       [(cons (list e1 e2 e3) q)
        (cons (If e1 e2 e3) q)])]
    [(Begin e1 e2)
     (match (intern-es (list e1 e2) q)
       [(cons (list e1 e2) q)
        (cons (Begin e1 e2) q)])]
    [(Let x e1 e2)
     (match (intern-es (list e1 e2) q)
       [(cons (list e1 e2) q)
        (cons (Let x e1 e2) q)])]
    [(App e1 es)
     (match (intern-es (cons e1 es) q)
       [(cons (cons e1 es) q)
        (cons (App e1 es) q)])]
    [(Lam f xs e)
     (match (intern-e e q)
       [(cons e q)
        (cons (Lam f xs e) q)])]
    [_ (cons e q)]))

;; [Listof Expr] QEnv -> (cons [Listof Expr] QEnv)
(define (intern-es es q)
  (match es
    ['() (cons '() q)]
    [(cons e es)
     (match (intern-e e q)
       [(cons e q)
        (match (intern-es es q)
          [(cons es q)
           (cons (cons e es) q)])])]))
