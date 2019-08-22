#lang racket
(provide (all-defined-out))

;; type REnv = (Listof (List Variable Integer))

;; Expr -> Integer
(define (dupe-interp e)
  (dupe-interp-env e '()))

;; Expr REnv -> Integer
(define (dupe-interp-env e r)
  (match e
    [(? integer? i) i]               ;; Abscond
    [`(add1 ,e0)                     ;; Blackmail
     (+ (dupe-interp-env e0 r) 1)]
    [`(sub1 ,e0)
     (- (dupe-interp-env e0 r) 1)]
    [`(if (zero? ,e0) ,e1 ,e2)       ;; Con
     (if (zero? e0)
         (dupe-interp-env e1 r)
         (dupe-interp-env e2 r))]
    [(? symbol? x)                   ;; Dupe
     (lookup r x)]
    [`(let ((,x ,e0)) ,e1)
     (let ((r0 (ext r x (dupe-interp-env e0 r))))
       (dupe-interp-env e1 r0))]))

;; Env Variable -> Integer
(define (lookup env x)
  (match env
    ['() (error "undefined variable:" x)]
    [(cons (list y i) env)
     (match (symbol=? x y)
       [#t i]
       [#f (lookup env x)])]))

;; Env Variable Integer -> Integer
(define (ext r x i)
  (cons (list x i) r))
