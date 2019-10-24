#lang racket

;; type TypeEnv = (Listof (List Variable Type))

;; type Type =
;; | 'Bool
;; | 'Int
;; | `(,Type -> ,Type)
;; | TVariable

;; type TVariable = Symbol (except 'Bool or 'Int)

;; type Expr =
;; | Integer
;; | Boolean
;; | `(λ (,Variable) ,Expr)
;; | `(,Expr ,Expr)

;; type Constraints = (Listof (List Type Type))
;; Interp: a set of equality constraints between each pair of types in the list


;; TypeEnv Expr -> (Maybe Type)
(define (type-infer G e)
  (match (type-constraints G e)
    [(list t C)
     (maybe-apply apply-solution (unify C) t)]))

;; TypeEnv Expr -> (List TVariable Constraints)
(define (type-constraints G e)
  (match e
    [#t '(Bool ())]
    [#f '(Bool ())]
    [(? integer?) '(Int ())]
    [(? symbol? x) (list (lookup x G) '())]
    [`(λ (,x) ,e)
     (let ((t1 (gensym)))
       (match (type-constraints (cons (list x t1) G) e)
         [(list t2 C)
          (let ((t (gensym)))
            (list t (append `((,t (,t1 -> ,t2))) C)))]))]
    [`(,e0 ,e1)
     (match (type-constraints G e0)
       [(list t0 C0)
        (match (type-constraints G e1)
          [(list t1 C1)
           (let ((t (gensym)))
             (list t (append `((,t0 (,t1 -> ,t))) C0 C1)))])])]))

;; Is x a type variable? (a symbol that's not 'Int or 'Bool)
(define (tvariable? x)
  (and (symbol? x)
       (not (memq x '(Bool Int)))))

;; type Subst = (Listof (List TVariable Type))
;; Invariant: no variable in lhs occurs in any term earlier in the list

;; Constraints -> (Maybe Subst)
(define (unify C)
  (match C
    ['() '()]
    [(cons (list T1 T2) C)
     (let ((s1 (unify C)))
       (let ((s2 (maybe-apply unify-one
                              (maybe-apply apply-solution s1 T1)
                              (maybe-apply apply-solution s1 T2))))
         (maybe-apply append s1 s2)))]))

;; Type Type -> (Maybe Subst)
;; Generate a substitution which unifies T1 and T2 (if possible)
(define (unify-one T1 T2)
  (if (eq? T1 T2)
      '()
      (match* (T1 T2)
        [(`(,T1 -> ,T2) `(,T3 -> ,T4))
         (unify `((,T1 ,T3) (,T2 ,T4)))]
        [((? tvariable? t) T)
         (and (not (occurs? t T))
              `((,t ,T)))]
        [(T (? tvariable? t)) 
         (and (not (occurs? t T))
              `((,t ,T)))]    
        [(_ _) #f])))

;; TVariable Type -> Boolean
;; Does t occur in T?
(define (occurs? t T)
  (match T
    [(? tvariable? y) (eq? t y)]
    [`(,T1 -> ,T2) (or (occurs? t T1)
                       (occurs? t T2))]
    [_ #f]))

;; X (Maybe (Listof X)) -> (Maybe (Listof X))
(define (maybe-cons x mxs)
  (and mxs (cons x mxs)))

;; (X ... -> Z) (Maybe X) ... -> (Maybe Z)
(define (maybe-apply f . xs)
  (and (andmap identity xs) (apply f xs)))


;; Solution Type -> Type
(define (apply-solution s T)
  (match s
    ['() T]
    [(cons (list t T*) s)
     (apply-solution s (subst-type T* t T))]))

;; Type Symbol Constaints -> Contraints
;; Substitute T for t in C
(define (subst T t C)
  (map (λ (c) (map (λ (T*) (subst-type T t T*)) c))               
       C))

;; Type Symbol Type -> Type
;; Substitute T for t in T*
(define (subst-type T t T*)
  (match T*
    ['Bool 'Bool]
    ['Int 'Int]
    [(? symbol? t*) (if (equal? t t*) T T*)]
    [`(,t1 -> ,t2)
     `(,(subst-type T t t1) -> ,(subst-type T t t2))]))



     





#|
;; Constraints -> Boolean
(define (unifiable? C)
  (match C
    ['() #t]
    [(cons (list (? variable? t) T) C)
     (unifiable? (subst T t C))]
    [(cons (list T (? variable? t)) C)
     (unifiable? (subst T t C))]
    [(cons (list T1 T2) C)
     (if (equal? T1 T2)
         (unifiable? C)
         (match* (T1 T2)
           [(`(,T3 -> ,T4) `(,T5 -> ,T6))
            (unifiable? (append `((,T3 ,T5) (,T4 ,T6)) C))]
           [(_ _) #f]))]))
|#



#|
;; type Type =
;; | 'Bool
;; | 'Int
;; | `(,Type -> ,Type)

;; type Expr =
;; | Integer
;; | Boolean
;; | `(λ (,Variable : ,Type) ,Expr)
;; | `(,Expr ,Expr)

;; TypeEnv Expr Type -> Bool
(define (type-check G e T)
  (equal? (type-infer G e) T))

;; TypeEnv Expr -> (Maybe Type)
(define (type-infer G e)
  (match e
    [#t 'Bool]
    [#f 'Bool]
    [(? symbol? x)
     (lookup x G)]
    [(? integer?) 'Int]
    [`(λ (,x : ,t) ,e)
     (match (type-infer (cons (list x t) G) e)
       [#f #f]
       [t2 `(,t -> ,t2)])]
    [`(,e0 ,e1)
     (match (type-infer G e0)
       [`(,t1 -> ,t2)
        (match (type-infer G e1)
          [#f #f]
          [t3 (and (equal? t1 t3) t2)])]
       [_ #f])]))
|#

;; TypeEnv Expr Type -> Boolean
#;
(define (type-check G e T)
  (match e
    [#t (match T
          ['Bool #t]
          [_ #f])]
    [#f (match T
          ['Bool #t]
          [_ #f])]
    [(? integer?)
     (match T
       ['Int #t]
       [_ #f])]

    [(? symbol? x)
     (equal? (lookup x G) T)]

    #;
    ;; Doesn't work because we don't know what T0 should be...
    [`(,e0 ,e1)
     (and
      (type-check G e0 `(,T0 -> ,T))
      (type-check G e1 ,T0))]

    [`(λ (,x) ,e)
     (match T
       [`(,T1 -> ,T2) (type-check (cons (list 'x T1) G) e T2)]
       [_ #f])]))

;; Variable TypeEnv -> (Maybe Type)
(define (lookup x G)
  (match G
    ['() #f]
    [(cons (list y T) G)
     (if (equal? x y)
         T
         (lookup x G))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Another approach: always infer

;; type Type =
;; | Symbol
;; | 'Bool
;; | 'Int
;; | `(,Type -> ,Type)












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Another approach: type annotations, bidirectional checking

;; type Expr =
;; | Integer
;; | Boolean
;; | `(λ (,Variable) ,Expr)
;; | `(,Expr ,Expr)
;; | `(: ,Expr ,Type)
#|
;; TypeEnv Expr Type -> (Maybe Type)
(define (type-check G e t)
  (and (equal? (type-infer G e) t) t))

;; TypeEnv Expr -> (Maybe Type)
(define (type-infer G e)
  (match e
    [#t 'Bool]
    [#f 'Bool]
    [_ #f]))

|#










































#|

;; TypeEnv Expr Type -> (Maybe Type)
(define (type-check G e t)
  (match e
    [`(λ (,x) ,e)
     (match t
       [`(,t0 -> ,t1)
        (and (type-check (cons (list x t0) G) e t1) t)]            
       [_ #f])]
    [_
     (and (equal? (type-infer G e) t) t)]))

;; TypeEnv Expr -> (Maybe Type)
(define (type-infer G e)
  (match e
    [#t 'Bool]
    [#f 'Bool]
    [(? integer?) 'Int]
    [(? symbol? x) (lookup x G)]
    [`(: ,e ,t) (type-check G e t)]
    [`(,e0 ,e1)
     (match (type-infer G e0)
       [`(,t0 -> ,t1) (and (type-check G e1 t0) t1)]
       [_ #f])]
    [_ #f]))

|#

         
     

     