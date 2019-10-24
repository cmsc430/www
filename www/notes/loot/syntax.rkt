#lang racket
(provide (all-defined-out))

;; Any -> Boolean
(define (imm? x)
  (or (integer? x)
      (boolean? x)
      (char? x)
      (equal? ''() x)))

;; Expr -> LExpr
(define (label-lambda e)
  (match e
    [(? symbol? x)         x]
    [(? imm? i)            i]
    [`(box ,e0)            `(box ,(label-lambda e0))]
    [`(unbox ,e0)          `(unbox ,(label-lambda e0))]
    [`(cons ,e0 ,e1)       `(cons ,(label-lambda e0) ,(label-lambda e1))]
    [`(car ,e0)            `(car ,(label-lambda e0))]
    [`(cdr ,e0)            `(cdr ,(label-lambda e0))]
    [`(add1 ,e0)           `(add1 ,(label-lambda e0))]
    [`(sub1 ,e0)           `(sub1 ,(label-lambda e0))]
    [`(zero? ,e0)          `(zero? ,(label-lambda e0))]
    [`(empty? ,e0)         `(empty? ,(label-lambda e0))]
    [`(if ,e0 ,e1 ,e2)     `(if ,(label-lambda e0) ,(label-lambda e1) ,(label-lambda e2))]
    [`(+ ,e0 ,e1)          `(+ ,(label-lambda e0) ,(label-lambda e1))]
    [`(let ((,x ,e0)) ,e1) `(let ((,x ,(label-lambda e0))) ,(label-lambda e1))]
    [`(λ ,xs ,e0)          `(λ ,xs ',(gensym) ,(label-lambda e0))]
    [`(,e . ,es)           `(,(label-lambda e) ,@(map label-lambda es))]))

;; LExpr -> (Listof LExpr)
;; Extract all the lambda expressions
(define (lambdas e)
  (match e
    [(? symbol? x)         '()]
    [(? imm? i)            '()]
    [`(box ,e0)            (lambdas e0)]
    [`(unbox ,e0)          (lambdas e0)]
    [`(cons ,e0 ,e1)       (append (lambdas e0) (lambdas e1))]
    [`(car ,e0)            (lambdas e0)]
    [`(cdr ,e0)            (lambdas e0)]
    [`(add1 ,e0)           (lambdas e0)]
    [`(sub1 ,e0)           (lambdas e0)]
    [`(zero? ,e0)          (lambdas e0)]
    [`(empty? ,e0)         (lambdas e0)]
    [`(if ,e0 ,e1 ,e2)     (append (lambdas e0) (lambdas e1) (lambdas e2))]
    [`(+ ,e0 ,e1)          (append (lambdas e0) (lambdas e1))]
    [`(let ((,x ,e0)) ,e1) (append (lambdas e0) (lambdas e1))]
    [`(λ ,xs ,l ,e0)       (cons e (lambdas e0))]
    [`(,e . ,es)           (append (lambdas e) (apply append (map lambdas es)))]))

;; LExpr -> (Listof Variable)
(define (fvs/unique e)  
  (remove-duplicates (fvs e)))

;; LExpr -> (Listof Variable)
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
    [`(λ ,xs ,l ,e0)       (remq* xs (fvs e0))]
    [`(,e . ,es)           (append (fvs e) (apply append (map fvs es)))]))
