#lang racket
(require "../compile.rkt"
         "../asm/interp.rkt"
         rackunit
         redex/reduction-semantics)

(define (run e)
  (asm-interp (compile e)))

(check-equal? (run 7) 7)
(check-equal? (run -8) -8)
(check-equal? (run '(add1 (add1 7))) 9)
(check-equal? (run '(add1 (sub1 7))) 7)

;; Examples from the notes
(check-equal? (run '(let ((x 7)) x)) 7)
(check-equal? (run '(let ((x 7)) 2)) 2)
(check-equal? (run '(let ((x 7)) (add1 x))) 8)
(check-equal? (run '(let ((x (add1 7))) x)) 8)
(check-equal? (run '(let ((x 7)) (let ((y 2)) x))) 7)
(check-equal? (run '(let ((x 7)) (let ((x 2)) x))) 2)
(check-equal? (run '(let ((x 7)) (let ((x (add1 x))) x))) 8)

; (check-equal? (run 'x) 'err)  ;; Not a valid program
(check-equal? (run '(add1 #f)) 'err)
(check-equal? (run '(+ 1 2)) 3)
(check-equal? (run '(zero? 0)) #t)
(check-equal? (run '(zero? 1)) #f)


;; Hustle tests
(check-equal? (run '(box 8)) (box 8))
(check-equal? (run '(unbox (box 8))) 8)
(check-equal? (run '(unbox 8)) 'err)

;; Iniquity tests
(check-equal? (run
               '(begin (define (f x) x)
                       (f 5)))
              5)

(check-equal? (run
               '(begin (define (tri x)
                         (if (zero? x)
                             0
                             (+ x (tri (sub1 x)))))
                       (tri 9)))
              45)

(check-equal? (run
               '(begin (define (even? x)
                         (if (zero? x)
                             #t
                             (odd? (sub1 x))))
                       (define (odd? x)
                         (if (zero? x)
                             #f
                             (even? (sub1 x))))
                       (even? 101)))
              #f)

(check-equal? (run
               '(begin (define (map-add1 xs)
                         (if (empty? xs)
                             '()
                             (cons (add1 (car xs))
                                   (map-add1 (cdr xs)))))
                       (map-add1 (cons 1 (cons 2 (cons 3 '()))))))
               '(2 3 4))

