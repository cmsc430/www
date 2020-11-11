#lang racket
(require "../compile.rkt" "../asm/interp.rkt" "../parse.rkt" rackunit)

(define (run e)
  (asm-interp (compile (parse e))))

(check-equal? (run 7) 7)
(check-equal? (run -8) -8)
(check-equal? (run '(add1 (add1 7))) 9)
(check-equal? (run '(add1 (sub1 7))) 7)


;; Con examples
(check-equal? (run '(if (zero? 0) 1 2)) 1)
(check-equal? (run '(if (zero? 1) 1 2)) 2)
(check-equal? (run '(if (zero? -7) 1 2)) 2)
(check-equal? (run '(if (zero? 0)
                        (if (zero? 1) 1 2)
                        7))
              2)
(check-equal? (run '(if (zero? (if (zero? 0) 1 0))
                        (if (zero? 1) 1 2)
                        7))
              7)

(check-equal? (run '(let ((x 7)) x)) 7)
(check-equal? (run '(let ((x 7)) 2)) 2)
(check-equal? (run '(let ((x 7)) (add1 x))) 8)
(check-equal? (run '(let ((x (add1 7))) x)) 8)
(check-equal? (run '(let ((x 7)) (let ((y 2)) x))) 7)
(check-equal? (run '(let ((x 7)) (let ((x 2)) x))) 2)
(check-equal? (run '(let ((x 7)) (let ((x (add1 x))) x))) 8)

(check-equal? (run '(let ((x 0))
                      (if (zero? x) 7 8)))
              7)
(check-equal? (run '(let ((x 1))
                      (add1 (if (zero? x) 7 8))))
              9)

(check-equal? (run '(+ 3 4)) 7)
(check-equal? (run '(- 3 4)) -1)
(check-equal? (run '(+ (+ 2 1) 4)) 7)
(check-equal? (run '(+ (+ 2 1) (+ 2 2))) 7)


