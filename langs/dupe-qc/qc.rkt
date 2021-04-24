#lang racket
(provide (all-defined-out))
(require quickcheck quickcheck/generator
         quickcheck/private/random)

; Let's test our ast!

(require "ast.rkt" "interp.rkt" "compile.rkt" "types.rkt" a86/interp)

(define (interp-compile-match e)
  (equal? (bits->value (asm-interp (compile e))) (interp e)))

(define (gen-term-aux sz)
  (if (= sz 0)
  ; Base case, non-recursive structures
  (choose-with-frequencies
   (list
  ; Int i
    (cons 1 (bind-generators
             ([i (choose-integer -42 42)])
             (Int i)))
  ; NEW in Dupe
  ; Bool b
    (cons 1 (bind-generators
             ([b (choose-one-of (list #t #f))])
             (Bool b)))))

  ; Recursive case, all options available
  (choose-with-frequencies
   (list

    ; (struct Int (i)           #:prefab)    
    (cons 1 (bind-generators
             ([i (choose-integer -42 42)])
             (Int i)))

    ; (struct Bool (b)      #:prefab)
    (cons 1 (bind-generators
             ([b (choose-one-of (list #t #f))])
             (Bool b)))
    
    ; (struct Prim1 (p e)       #:prefab)
    ;; type Op = 'add1 | 'sub1 | 'zero?
    (cons 1 (bind-generators
             ([p (choose-one-of (list 'add1 'sub1 'zero?))]
              [e (gen-term-aux (sub1 sz))])
             (Prim1 p e)))

    ; (struct IfZero (e1 e2 e3) #:prefab)
    (cons 1 (bind-generators
             ([e1 (gen-term-aux (sub1 sz))]
              [e2 (gen-term-aux (sub1 sz))]
              [e3 (gen-term-aux (sub1 sz))])
             (If e1 e2 e3)))
    ))))

(define prop-interp-compile
  (property ([e (gen-term-aux 4)])
            (interp-compile-match e)))

;(quickcheck prop-interp-compile)

(define (shrink-loop shr p e)
  (displayln e)
  (define (loop-shrinks vs)
    (match vs
      ['() e]
      [(cons v vs) (if (p v) (loop-shrinks vs)
                       (shrink-loop shr p v))]))
  (loop-shrinks (shr e)))

(define (quickcheck-shrink shr prop propb)
  (define-values (ntests stamp result) (quickcheck-results prop))
  (shrink-loop shr propb (cdaar (result-arguments-list result))))

;; TODO: More reasonable shrinking
(define (shrink-int i)
  (if (= i 0) '()
      (if (= i 1) (list 0)
          (list 0 1))))

(define (shrink-op1 p)
  (match p
    ['add1 '()]
    ['sub1 (list add1)]
    ['zero? '()]
    ))

(define (shrink-term e)
  (match e
    [(Int i) (map (lambda (x) (Int x)) (shrink-int i))]
    [(Prim1 p e) (append (list e)
                         (map (lambda (ps) (Prim1 ps e)) (shrink-op1 p))
                         (map (lambda (es) (Prim1 p es)) (shrink-term e)))]
    [(If e1 e2 e3)
     (append (list e2 e3)
             (map (lambda (es) (If es e2 e3)) (shrink-term e1))
             (map (lambda (es) (If e1 es e3)) (shrink-term e2))
             (map (lambda (es) (If e1 e2 es)) (shrink-term e3)))]))

; (quickcheck-shrink shrink-term prop-interp-compile interp-compile-match)

 






