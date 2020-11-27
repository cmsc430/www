#lang racket
(provide (all-defined-out))
(require "syntax.rkt"
         "interp-env.rkt")


(define (interp e)
  (interp-env (desugar e) stdlib))

(define stdlib
  `((append ,append)
    (list? ,list?)
    (first ,first)
    (second ,second)
    (rest ,rest)
    (reverse ,reverse)
    (not ,not)
    (compose ,compose)
    (symbol=? ,symbol=?)
    (memq ,memq)
    (length ,length)
    (remq* ,remq*)
    (remove-duplicates ,remove-duplicates)
    (remove ,remove)
    (member ,member)
    (equal? ,equal?)))


;; Expr REnv Natural -> Answer
(define (interp-qq d r n)
  ;(println `(interp-qq ,d ,n))
  (match d
    [`(,'unquote ,e)
     (if (zero? n)
         (interp-env (desugar e) r) ;!
         (cons 'unquote (interp-qq-list e r (sub1 n))))]
    [`(,'unquote-splicing ,e) 'err]
    [`(,'quasiquote ,d)
     (cons 'quasiquote (interp-qq-list d r (add1 n)))]
    [`(,x . ,y)
     (match (interp-qq-list x r n)
       ['err 'err]
       [xv (match (interp-qq y r n)
             ['err 'err]
             ['() xv]
             [yv (if (list? xv)
                     (append xv yv)
                     'err)])])]
    [d d]))

;; Expr REnv Natural -> Answer
(define (interp-qq-list d r n)
  ;(println `(interp-qq-list ,d ,n))
  (match d
    [`(,'unquote ,e)
     (if (zero? n)
         (match (interp-env (desugar e) r) ;!
           ['err 'err]
           [v (list v)])
         (list (cons 'unquote (interp-qq-list e r (sub1 n)))))]
    [`(,'unquote-splicing ,e)
     (if (zero? n)
         (interp-env e r)
         (list (cons 'unquote-splicing (interp-qq-list e r (sub1 n)))))]
    [`(,'quasiquote ,d)
     (list (cons 'quasiquote (interp-qq-list d r (add1 n))))]
    [`(,x . ,y)
     (match (interp-qq-list x r n)
       ['err 'err]
       [xv (match (interp-qq y r n)
             ['err 'err]
             [yv (list (append xv yv))])])]
    [d (list d)]))

    
