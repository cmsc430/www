#lang racket
(provide match->cond)

;; type Expr+ =
;; ....
;; | Match

;; type Match = (match ,Expr+ ,(list Pat Expr+) ...)

;; type Pat =
;; | #t
;; | #f
;; | Integer
;; | String
;; | '()
;; | Variable
;; | `(quote ,Symbol)
;; | `(cons ,Pat ,Pat)
;; | `(list ,Pat ...)
;; | `(? ,Expr ,Pat ...)

;; Match -> Expr
;; Rewrite match expression into an equivalent cond expression
(define (match->cond m)
  (match m
    [`(match ,e . ,mcs)
     (let ((x (gensym)))
       `(let ((,x ,e))
          (cond ,@(map (λ (mc)
                         (match mc
                           [(list p e)
                            (list (pat-match p x) (pat-bind p x e))]))
                       mcs)
                ;; fall through to error
                [else (car '())])))]))

;; Example
#;
(define (sum bt)
  (match bt
    ['leaf 0]
    [(list 'node v l r)
     (+ v
        (+ (sum l)
           (sum r)))]))
#;
(define (sum^ bt)
  (cond
    [(eq? 'leaf bt) 0]
    [(and (list? bt)
          (= 4 (length bt))
          (eq? 'node (first bt)))
     (let ((v (second bt))
           (l (third bt))
           (r (fourth bt)))
       (+ v
          (+ (sum l)
             (sum r))))]))

#;
`(define (sum bt)
   ,(match->cond
     '(match bt
        ['leaf 0]
        [(list 'node v l r)
         (+ v
            (+ (sum l)
               (sum r)))])))

;; Two tasks:
;; 1. rewrite patterns into Boolean valued expressions that answer
;;    whether the pattern matches the scrutiny
;; 2. rewrite pattern and RHS in to expressions in which the pattern variables
;;    of pattern are bound to the appropriately deconstructed parts of the scrutiny

;; Assume: the scrutiny is a variable.
;; (It's easy to establish this assumption in general.)

;; Two functions:

#;
;; Pat Variable -> Expr
;; Produces an expression determining if p matches v
(define (pat-match p v) ...)

#;
;; Pat Variable Expr -> Expr
;; Produce an expression that deconstructs v and binds pattern variables
;; of p in scope of e.
;; ASSUME: v matches p
(define (pat-bind p v e) ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern matching

;; Pat Variable -> Expr
;; Produces an expression determining if p matches v
(define (pat-match p v)
  (match p
    [#t `(eq? #t ,v)]
    [#f `(eq? #f ,v)]
    [(? integer? i) `(eq? ,i ,v)]
    [(? string? s)
     `(and (string? ,v)
           (string=? ,s ,v))]
    [''() `(eq? '() ,v)]
    [(? symbol?) #t]
    [`',(? symbol? s) `(eq? ,v ',s)]
    [`(cons ,p1 ,p2)
     (let ((v1 (gensym))
           (v2 (gensym)))
       `(and (cons? ,v)
             (let ((,v1 (car ,v))
                   (,v2 (cdr ,v)))             
               (and ,(pat-match p1 v1)
                    ,(pat-match p2 v2)))))]
    [`(list . ,ps)
     `(and (list? ,v)
           (= (length ,v) ,(length ps))
           ,(pat-match-list ps v))]
    [`(? ,e . ,ps)
     `(and (,e ,v)
           ,(pats-match ps v))]))

;; (Listof Pat) Variable -> Expr
;; Produces an expression determining if every ps matches x
(define (pats-match ps v)
  (match ps
    ['() #t]
    [(cons p ps)
     `(and ,(pat-match p v)
           ,(pats-match ps v))]))

;; (Listof Pat) Variable -> Expr
;; Produces an expression determining if each ps matches each element of list v
(define (pat-match-list ps v)
  (match ps
    ['() #t]
    [(cons p ps)
     (let ((v1 (gensym))
           (v2 (gensym)))
       `(let ((,v1 (car ,v))
              (,v2 (cdr ,v)))
          (and ,(pat-match p v1)
               ,(pat-match-list ps v2))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern binding

;; Pat Variable Expr -> Expr
;; Produce an expression that deconstructs v and binds pattern variables
;; of p in scope of e.
;; ASSUME: v matches p
(define (pat-bind p v e)
  (match p
    [#t e]
    [#f e]
    [(? integer?) e]
    [(? string?) e]
    [''() e]
    [(? symbol? x) `(let ((,x ,v)) ,e)]
    [`',(? symbol?) e]
    [`(cons ,p1 ,p2)
     (let ((v1 (gensym))
           (v2 (gensym)))
       `(let ((,v1 (car ,v))
              (,v2 (cdr ,v)))
          ,(pat-bind p1 v1
                     (pat-bind p2 v2 e))))]
    [`(list . ,ps)
     (pat-bind-list ps v e)]
    [`(? ,_ . ,ps)
     (pats-bind ps v e)]))

;; (Listof Pat) Variable Expr -> Expr
;; Produce an expression that doconstructs v and binds pattern variables
;; of ps (each matched against v) in scope of e.
;; ASSUME: v matches every element of ps
(define (pats-bind ps v e)
  (match ps
    ['() e]
    [(cons p ps)
     (pat-bind p v (pats-bind ps v e))]))

;; (Listof Pat) Variable Expr -> Expr
;; Produce an expression that deconstructs list v and binds pattern variables
;; of ps (matched element-wise against v) in scope of e.
;; ASSUME: elemens of v matches elements of ps
(define (pat-bind-list ps v e)
  (match ps
    ['() e]
    [(cons p ps)
     (let ((v1 (gensym))
           (v2 (gensym)))
       `(let ((,v1 (car ,v))
              (,v2 (cdr ,v)))          
          ,(pat-bind p v1 (pat-bind-list ps v2 e))))]))
