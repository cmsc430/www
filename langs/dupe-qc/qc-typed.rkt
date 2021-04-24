#lang racket
(provide (all-defined-out))
(require quickcheck quickcheck/generator
         quickcheck/private/random)

; Let's test our ast!

(require "ast.rkt" "interp.rkt" "compile.rkt" "types.rkt" a86/interp)

(define (interp-compile-match e)
  (equal? (bits->value (asm-interp (compile e))) (interp e)))

; type : 'type-int | 'type-bool
(define (gen-term-aux type sz)
  (if (= sz 0)
  ; Base case, non-recursive structures
      (match type
        ['type-int
         (bind-generators
                  ([i (choose-integer -42 42)])
                  (Int i))]
        ['type-bool
         (bind-generators
                  ([b (choose-one-of (list #t #f))])
                  (Bool b))])
  ; Recursive case, all options available
  (choose-with-frequencies
   (append

    ; (struct Int (i)           #:prefab)
    (list
     (match type
      ['type-int 
       (cons 1 (bind-generators
                      ([i (choose-integer -42 42)])
                      (Int i)))]
      ['type-bool
         (cons 1 (bind-generators
                  ([b (choose-one-of (list #t #f))])
                  (Bool b)))]))
    ; (struct Int (i)           #:prefab)

    ; (struct Prim1 (p e)       #:prefab)
    ;; type Op = 'add1
     (list (match type
             ['type-int
              (cons 2
                    (bind-generators
                     ([p (choose-one-of (list 'add1 'sub1))]
                      [e (gen-term-aux 'type-int (sub1 sz))])
                     (Prim1 p e)))]
             ['type-bool
              (cons 3 (bind-generators
                       ([e (gen-term-aux 'type-int (sub1 sz))])
                       (Prim1 'zero? e)))]))
     
    ; (struct If (e1 e2 e3) #:prefab)
    (list (cons 1 (bind-generators
                   ([e1 (gen-term-aux 'type-bool (sub1 sz))]
                    [e2 (gen-term-aux type (sub1 sz))]
                    [e3 (gen-term-aux type (sub1 sz))])
                   (If e1 e2 e3))))
    ))))

(define prop-interp-compile
  (property ([e (gen-term-aux 'type-int 4)])
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
  (match result
    [#t (begin (display "Succesfully ran ") (display ntests) (displayln " tests.")) ]
    [(? result?) (shrink-loop shr propb (cdaar (result-arguments-list result)))]))

;; TODO: More reasonable shrinking
(define (shrink-int i)
  (if (= i 0) '()
      (if (= i 1) (list 0)
          (list 0 1))))

(define (shrink-op1 p)
  (match p
    ['add1 (list add1)]
    ['sub1 (list add1)]
    ['zero? '()]
    ))

(define (shrink-bool b)
  (match b
    [#t '()]
    [#f (list #t)]))

(define (shrink-term e)
  (match e
    [(Int  i) (map (lambda (x) (Int x)) (shrink-int i))]
    [(Bool b) (map (lambda (x) (Bool x)) (shrink-bool b))]
    [(Prim1 p e) (append (list e)
                         (map (lambda (ps) (Prim1 ps e)) (shrink-op1 p))
                         (map (lambda (es) (Prim1 p es)) (shrink-term e)))]
    [(If e1 e2 e3)
     (append (list e2 e3)
             (map (lambda (es) (If es e2 e3)) (shrink-term e1))
             (map (lambda (es) (If e1 es e3)) (shrink-term e2))
             (map (lambda (es) (If e1 e2 es)) (shrink-term e3)))]))

(quickcheck-shrink shrink-term prop-interp-compile interp-compile-match)

 










