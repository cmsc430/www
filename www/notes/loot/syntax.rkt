#lang racket
(provide (all-defined-out))

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
      [`(λ ,xs ,l ,e0)       (remq* xs (fvs e0))]
      [`(,e . ,es)           (append (fvs e) (apply append (map fvs es)))]))          
  (remove-duplicates (fvs e)))
