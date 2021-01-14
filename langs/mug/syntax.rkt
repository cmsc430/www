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
    [`',d                  (quote->expr d)]
    [`(,(? prim? p) . ,es) `(,p ,@(map desugar es))]
    [`(if ,e0 ,e1 ,e2)     `(if ,(desugar e0) ,(desugar e1) ,(desugar e2))]
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
;; use of quote (except for symbols and empty list)
(define (quote->expr d)
  (match d
    [(? boolean?) d]
    [(? integer?) d]
    [(? string?) d]
    [(? char?) d]
    [(? symbol?) (list 'quote d)]
    [(cons x y) (list 'cons (quote->expr x) (quote->expr y))]
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

;; Expr -> Expr
(define (cond->if c)
  (match c
    [`(cond (else ,e)) e]
    [`(cond (,c ,e) . ,r)
     `(if ,c ,e (cond ,@r))]))

;; Expr -> Expr
(define (and->if c)
  (match c
    [`(and) #t]
    [`(and ,e) e]
    [`(and ,e . ,r)
     `(if ,e (and ,@r) #f)]))

;; Expr -> Expr
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
    [`(,(? prim? p) . ,es) `(,p ,@(map label-λ es))]
    [`(if ,e0 ,e1 ,e2)     `(if ,(label-λ e0) ,(label-λ e1) ,(label-λ e2))]
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
    [`(,(? prim? p) . ,es) (append-map λs es)]
    [`(if ,e0 ,e1 ,e2)     (append (λs e0) (λs e1) (λs e2))]
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
      [`(,(? prim? p) . ,es) (append-map fvs es)]
      [`(if ,e0 ,e1 ,e2)     (append (fvs e0) (fvs e1) (fvs e2))]
      [`(let ((,x ,e0)) ,e1) (append (fvs e0) (remq* (list x) (fvs e1)))]
      [`(letrec ,bs ,e0)     (remq* (map first bs)
                                    (apply append (fvs e0) (map fvs (map second bs))))]
      [`(λ ,xs ,l ,e0)       (remq* xs (fvs e0))]
      [`(,e . ,es)           (append (fvs e) (apply append (map fvs es)))]))
  (remove-duplicates (fvs e)))

;; Any -> Boolean
(define (prim? x)
  (and (symbol? x)
       (memq x '(add1 sub1 zero? abs - char? boolean? integer? integer->char char->integer
                      string? box? empty? cons cons? box unbox car cdr string-length
                      make-string string-ref = < <= char=? boolean=? + eq? gensym symbol?
                      procedure?))))
