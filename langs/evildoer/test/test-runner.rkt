#lang racket
(provide test test/io)
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
    (check-equal? (run '(zero? 0)) #t))

  (begin ;; Dodger
    (check-equal? (run #\a) #\a)
    (check-equal? (run #\b) #\b)
    (check-equal? (run '(char? #\a)) #t)
    (check-equal? (run '(char? #t)) #f)
    (check-equal? (run '(char? 8)) #f)
    (check-equal? (run '(char->integer #\a)) (char->integer #\a))
    (check-equal? (run '(integer->char 955)) #\λ))

  (begin ;; Evildoer
    (check-equal? (run '(void)) (void))
    (check-equal? (run '(begin 1 2)) 2)
    (check-equal? (run '(eof-object? (void))) #f)))

(define (test/io run)
  (begin ;; Evildoer
    (check-equal? (run "" 7) (cons 7 ""))
    (check-equal? (run "" '(write-byte 97)) (cons (void) "a"))
    (check-equal? (run "a" '(read-byte)) (cons 97 ""))
    (check-equal? (run "b" '(begin (write-byte 97) (read-byte)))
                  (cons 98 "a"))
    (check-equal? (run "" '(read-byte)) (cons eof ""))
    (check-equal? (run "" '(eof-object? (read-byte))) (cons #t ""))
    (check-equal? (run "a" '(eof-object? (read-byte))) (cons #f ""))
    (check-equal? (run "" '(begin (write-byte 97) (write-byte 98)))
                  (cons (void) "ab"))

    (check-equal? (run "ab" '(peek-byte)) (cons 97 ""))
    (check-equal? (run "ab" '(begin (peek-byte) (read-byte))) (cons 97 ""))
    (check-equal? (run "†" '(read-byte)) (cons 226 ""))
    (check-equal? (run "†" '(peek-byte)) (cons 226 ""))))

