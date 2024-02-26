#lang racket
(provide test)
(require rackunit)

(define (test run)
  (begin ;; Abscond
    (check-equal? (run 7) 7)
    (check-equal? (run -8) -8))

  (begin ;; Blackmail
    (check-equal? (run '(add1 (add1 7))) 9)
    (check-equal? (run '(add1 (sub1 7))) 7))

  (begin ;; Con
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
                  7))

  (begin ;; Dupe
    (check-equal? (run #t) #t)
    (check-equal? (run #f) #f)
    (check-equal? (run '(if #t 1 2)) 1)
    (check-equal? (run '(if #f 1 2)) 2)
    (check-equal? (run '(if 0 1 2)) 1)
    (check-equal? (run '(if #t 3 4)) 3)
    (check-equal? (run '(if #f 3 4)) 4)
    (check-equal? (run '(if  0 3 4)) 3)
    (check-equal? (run '(zero? 4)) #f)
    (check-equal? (run '(zero? 0)) #t)))

