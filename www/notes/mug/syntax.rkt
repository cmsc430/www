#lang racket
(provide (all-defined-out))
(require "pat.rkt")

;; type Expr+ =
;; .... exprs with match, cond, begin/define, quote etc.

;; type S-Expr =
;; | Boolean
;; | Integer
;; | String
;; | '()
;; | (Cons S-Expr S-Expr)

;; Expr+ -> Expr
(define (desugar e+)
  (match e+
    [`(begin ,@(list `(define (,fs . ,xss) ,es) ...) ,e)
     `(letrec ,(map (λ (f xs e) `(,f (λ ,xs ,(desugar e)))) fs xss es)
        ,(desugar e))]    
    [(? symbol? x)         x]
    [(? imm? i)            i]
    [`',(? symbol? s)      `',s]
    [`',d                  (desugar (quote->expr d))]       ;; quote & quasiquote as expansions
    [(list 'quasiquote d)  (desugar (quasiquote->expr d))]
    [`(box ,e0)            `(box ,(desugar e0))]
    [`(unbox ,e0)          `(unbox ,(desugar e0))]
    [`(cons ,e0 ,e1)       `(cons ,(desugar e0) ,(desugar e1))]
    [`(car ,e0)            `(car ,(desugar e0))]
    [`(cdr ,e0)            `(cdr ,(desugar e0))]
    [`(add1 ,e0)           `(add1 ,(desugar e0))]
    [`(sub1 ,e0)           `(sub1 ,(desugar e0))]
    [`(zero? ,e0)          `(zero? ,(desugar e0))]
    [`(empty? ,e0)         `(empty? ,(desugar e0))]
    [`(if ,e0 ,e1 ,e2)     `(if ,(desugar e0) ,(desugar e1) ,(desugar e2))]
    [`(+ ,e0 ,e1)          `(+ ,(desugar e0) ,(desugar e1))]
    [`(let ((,x ,e0)) ,e1) `(let ((,x ,(desugar e0))) ,(desugar e1))]
    [`(letrec ,bs ,e0)
     `(letrec ,(map (λ (b) (list (first b) (desugar (second b)))) bs)
        ,(desugar e0))]
    [`(λ ,xs ,e0)          `(λ ,xs ,(desugar e0))]
    [`(match . ,_)         (desugar (match->cond e+))]
    [`(cond . ,_)          (desugar (cond->if e+))]
    [`(and . ,_)           (desugar (and->if e+))]
    [`(or . ,_)            (desugar (or->if e+))]
    [`(,e . ,es)           `(,(desugar e) ,@(map desugar es))]))

;; S-Expr -> Expr
;; Produce an expression that evaluates to given s-expression, without
;; use of quote (except for symbols)
(define (quote->expr d)
  (match d
    [(? boolean?) d]
    [(? integer?) d]
    [(? string?) d]
    [(? char?) d]
    [(? symbol?) (list 'quote d)]
    ;[(cons x y) (list 'cons (quote->expr x) (quote->expr y))]
    [(cons x y) (list 'cons (list 'quote x) (list 'quote y))]
    ['() ''()]))

(define (quasiquote->expr d) 
  (match d
    [(? boolean?) d]
    [(? integer?) d]
    [(? string?) d]
    [(? char?) d]
    [(? symbol?) (list 'quote d)]
    [(cons 'quasiquote d)
     (quasiquote->expr (quasiquote->expr d))]
    [(cons 'unquote d) d]
    [(cons 'unquote-splicing d) 'ERROR]   
    [(cons x y)
     `(append ,(quasiquote->list-expr x)
              ,(quasiquote->expr y))]
    ['() ''()]))

(define (quasiquote->list-expr d) 
  (match d    
    [(? symbol?) (list 'quote d)]
    ['() ''()]
    [(cons 'quasiquote d)
     (quasiquote->expr (quasiquote->expr d))]
    [(cons 'unquote d) `(list ,d)]
    [(cons 'unquote-splicing d) d]  
    [(cons x y)
     `(list (append ,(quasiquote->list-expr x)
                    ,(quasiquote->expr y)))]
    [_ `'(,d)]))

(define (cond->if c)
  (match c
    [`(cond (else ,e)) e]
    [`(cond (,c ,e) . ,r)
     `(if ,c ,e (cond ,@r))]))

(define (and->if c)
  (match c
    [`(and) #t]
    [`(and ,e) e]
    [`(and ,e . ,r)
     `(if ,e (and ,@r) #f)]))

(define (or->if c)
  (match c
    [`(or) #f]
    [`(or ,e) e]
    [`(or ,e . ,r)
     (let ((x (gensym)))
       `(let ((,x ,e))
          (if ,x ,x (or ,@r))))]))


(define (qq-expand x depth)
  (match x
    [(cons 'quasiquote r)
     `(cons 'quasiquote ,(qq-expand r (add1 depth)))]
    [(cons 'unquote r)
     (cond [(> depth 0)
            `(cons ','unquote ,(qq-expand r (sub1 depth)))]
           [(and (not (empty? r))
                 (empty? (cdr r)))
            (car r)]
           [else
            (error "Illegal")])]
    [(cons 'unqupte-splicing r)
     (error "Illegal")]
    [(cons a b)
     `(append ,(qq-expand-list a depth)
              ,(qq-expand b depth))]
    [_ `',x]))

(define (qq-expand-list x depth)
  (match x
    [(cons 'quasiquote r)
     `(list (cons 'quasiquote ,(qq-expand r (add1 depth))))]
    [(cons 'unquote r)
     (cond [(> depth 0) `(list (cons ','unquote ,(qq-expand r (sub1 depth))))]
           [else `(list . ,r)])]
    [(cons 'unquote-splicing r)
     (cond [(> depth 0) `(list (cons ','unquote-splicing ,(qq-expand r (sub1 depth))))]
           [else `(append . ,r)])]
    [_
     `'(,x)]))
      


;; Any -> Boolean
(define (imm? x)
  (or (integer? x)
      (boolean? x)
      (char? x)
      (equal? ''() x)))

;; Expr -> LExpr
(define (label-λ e)
  (match e
    [(? symbol? x)         x]
    [(? imm? i)            i]
    [`(box ,e0)            `(box ,(label-λ e0))]
    [`(unbox ,e0)          `(unbox ,(label-λ e0))]
    [`(cons ,e0 ,e1)       `(cons ,(label-λ e0) ,(label-λ e1))]
    [`(car ,e0)            `(car ,(label-λ e0))]
    [`(cdr ,e0)            `(cdr ,(label-λ e0))]
    [`(add1 ,e0)           `(add1 ,(label-λ e0))]
    [`(sub1 ,e0)           `(sub1 ,(label-λ e0))]
    [`(zero? ,e0)          `(zero? ,(label-λ e0))]
    [`(empty? ,e0)         `(empty? ,(label-λ e0))]
    [`(if ,e0 ,e1 ,e2)     `(if ,(label-λ e0) ,(label-λ e1) ,(label-λ e2))]
    [`(+ ,e0 ,e1)          `(+ ,(label-λ e0) ,(label-λ e1))]
    [`(let ((,x ,e0)) ,e1) `(let ((,x ,(label-λ e0))) ,(label-λ e1))]
    [`(letrec ,bs ,e0)     `(letrec ,(map (λ (b) (list (first b) (label-λ (second b)))) bs)
                              ,(label-λ e0))]
    [`(λ ,xs ,e0)          `(λ ,xs ',(gensym) ,(label-λ e0))]    
    [`(,e . ,es)           `(,(label-λ e) ,@(map label-λ es))]))

;; LExpr -> (Listof LExpr)
;; Extract all the lambda expressions
(define (λs e)
  (match e
    [(? symbol? x)         '()]
    [(? imm? i)            '()]
    [`(box ,e0)            (λs e0)]
    [`(unbox ,e0)          (λs e0)]
    [`(cons ,e0 ,e1)       (append (λs e0) (λs e1))]
    [`(car ,e0)            (λs e0)]
    [`(cdr ,e0)            (λs e0)]
    [`(add1 ,e0)           (λs e0)]
    [`(sub1 ,e0)           (λs e0)]
    [`(zero? ,e0)          (λs e0)]
    [`(empty? ,e0)         (λs e0)]
    [`(if ,e0 ,e1 ,e2)     (append (λs e0) (λs e1) (λs e2))]
    [`(+ ,e0 ,e1)          (append (λs e0) (λs e1))]
    [`(let ((,x ,e0)) ,e1) (append (λs e0) (λs e1))]
    [`(letrec ,bs ,e0)     (append (apply append (map (compose λs second) bs)) (λs e0))]
    [`(λ ,xs ,l ,e0)       (cons e (λs e0))]
    [`(,e . ,es)           (append (λs e) (apply append (map λs es)))]))

;; LExpr -> (Listof Variable)
(define (fvs e)
  (define (fvs e)
    (match e
      [(? symbol? x)         (list x)]
      [(? imm? i)            '()]
      [`(box ,e0)            (fvs e0)]
      [`(unbox ,e0)          (fvs e0)]
      [`(cons ,e0 ,e1)       (append (fvs e0) (fvs e1))]
      [`(car ,e0)            (fvs e0)]
      [`(cdr ,e0)            (fvs e0)]
      [`(add1 ,e0)           (fvs e0)]
      [`(sub1 ,e0)           (fvs e0)]
      [`(zero? ,e0)          (fvs e0)]
      [`(empty? ,e0)         (fvs e0)]
      [`(if ,e0 ,e1 ,e2)     (append (fvs e0) (fvs e1) (fvs e2))]
      [`(+ ,e0 ,e1)          (append (fvs e0) (fvs e1))]
      [`(let ((,x ,e0)) ,e1) (append (fvs e0) (remq* (list x) (fvs e1)))]
      [`(letrec ,bs ,e0)     (remq* (map first bs)
                                    (apply append (fvs e0) (map fvs (map second bs))))]      
      [`(λ ,xs ,l ,e0)       (remq* xs (fvs e0))]
      [`(,e . ,es)           (append (fvs e) (apply append (map fvs es)))]))          
  (remove-duplicates (fvs e)))
