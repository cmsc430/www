#lang racket
(provide (all-defined-out))

;; type REnv = (Listof (List Variable Integer))

;; Expr -> Integer
(define (interp e)
  (interp-env e '()))

;; Expr REnv -> Integer
(define (interp-env e r)
  (match e
    [(? integer? i) i]               ;; Abscond
    [`(add1 ,e0)                     ;; Blackmail
     (+ (interp-env e0 r) 1)]
    [`(sub1 ,e0)
     (- (interp-env e0 r) 1)]
    [`(if (zero? ,e0) ,e1 ,e2)       ;; Con
     (if (zero? e0)
         (interp-env e1 r)
         (interp-env e2 r))]
    [(? symbol? x)                   ;; Dupe
     (lookup r x)]
    [`(let ((,x ,e0)) ,e1)
     (let ((r0 (ext r x (interp-env e0 r))))
       (interp-env e1 r0))]))

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
