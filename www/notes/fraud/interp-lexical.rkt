#lang racket
(provide (all-defined-out))
(require (only-in "interp.rkt" prim? value? interp-prim))

;; type LEnv = (Listof Variable)
;; type VEnv = (Listof Value)

;; Expr -> Answer
(define (interp e)
  (interp-env (translate e) '()))

;; Expr -> IExpr
(define (translate e)
  (translate-e e '()))

;; Expr LEnv -> IExpr
(define (translate-e e r)
  (match e
    [(? value? v) v]
    [(list (? prim? p) e)
     (list p (translate-e e r))]
    [`(if ,e0 ,e1 ,e2)
     `(if ,(translate-e e0)
          ,(translate-e e1)
          ,(translate-e e2))]
    [(? symbol? x)
     `(address ,(lexical-address x r))]
    [`(let ((,x ,e0)) ,e1)
     `(let ((_ ,(translate-e e0 r)))
       ,(translate-e e1 (cons x r)))]))

;; IExpr VEnv -> Answer
(define (interp-env e r)
  (match e
    [(? value? v) v]
    [(list (? prim? p) e)
     (let ((a (interp-env e r)))
       (interp-prim p a))]
    [`(if ,e0 ,e1 ,e2)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [`(address ,i)
     (list-ref r i)]
    [`(let ((_ ,e0)) ,e1)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (interp-env e1 (cons v r))])]))

;; Variable LEnv -> Natural
(define (lexical-address x r)
  (match r
    ['() (error "unbound variable")]
    [(cons y r)
     (match (symbol=? x y)
       [#t 0]
       [#f (add1 (lexical-address x r))])]))

(module+ test
  (require rackunit)
  (check-equal? (translate '(let ((x 0)) x))
                '(let ((_ 0)) (address 0)))
  (check-equal? (translate '(let ((x 0)) (let ((y 1)) x)))
                '(let ((_ 0)) (let ((_ 1)) (address 1))))
  (check-equal? (translate '(let ((x 0)) (let ((y 1)) y)))
                '(let ((_ 0)) (let ((_ 1)) (address 0))))
  (check-equal? (translate '(let ((x 0))
                              (let ((y x))
                                y)))
                '(let ((_ 0)) (let ((_ (address 0))) (address 0))))

  (check-equal? (interp 5) 5)
  (check-equal? (interp '(let ((x 0)) x)) 0)
  (check-equal? (interp '(let ((x 0)) (let ((y 1)) x))) 0)
  (check-equal? (interp '(let ((x 0)) (let ((y 1)) y))) 1)
  (check-equal? (interp '(let ((x 0)) (let ((y x)) y))) 0))
