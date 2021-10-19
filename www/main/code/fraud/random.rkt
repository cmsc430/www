#lang racket
(provide (all-defined-out))
(require "parse.rkt")

;; Randomly generate an expression
;; Note: this will often generate programs with type errors
(define (random-expr)
  (parse
   (close (random-open-expr))))

;; Randomly generate an expression (often with free variables)
(define (random-open-expr)
  (contract-random-generate
   (flat-rec-contract e
                      #t
                      #f
                      simple-symbol/c
                      (integer-in #f #f)
                      (list/c 'add1 e)
                      (list/c 'sub1 e)
                      (list/c 'zero? e)
                      (list/c 'if e e e)
                      (list/c 'let (list/c (list/c simple-symbol/c e)) e)
                      (list/c '+ e e)
                      (list/c '- e e))))
                       

;; Take an expression and close it (randomly)
(define (close e)
  (close-bv e '()))

;; Replace any variable with a randomly chosen bound variable (if any)
;; otherwise replace with a random let- and var-free expression
(define (close-bv e bvs)
  (match e
    [(? symbol?)
     (match bvs
       ['() (random-letless-expr)]
       [_ (pick-random bvs)])]
    [(? integer?) e]
    [(? boolean?) e]
    [`(let ((,x ,e0)) ,e1)
     `(let ((,x ,(close-bv e0 bvs)))
        ,(close-bv e1 (cons x bvs)))]
    [(cons s es)
     (cons s (map (lambda (e) (close-bv e bvs)) es))]))

(define (pick-random xs)
  (list-ref xs (random (length xs))))

;; list of bound variables
(define (bvs e)
  (match e
    [`(let ((,x ,e0)) ,e1)
     (append (list x) (bvs e0) (bvs e1))]
    [(cons (? symbol?) es)
     (apply append (map bvs es))]
    [_ '()]))

;; '(a0 b0 ... yj zj)
(define syms
  (for*/list ([i (in-range 97 (+ 97 26))]
              [j 3])
    (string->symbol (format "~a~a" (integer->char i) j))))

(define simple-symbol/c
  (apply first-or/c syms))

;; Randomly generate an extort expression (no lets or vars)
(define (random-letless-expr)
  (contract-random-generate
   (flat-rec-contract e
                      #t
                      #f
                      (integer-in #f #f)
                      (list/c 'add1 e)
                      (list/c 'sub1 e)
                      (list/c 'zero? e)
                      (list/c 'if e e e)
                      (list/c '+ e e)
                      (list/c '- e e))))

