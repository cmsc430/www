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
    (check-equal? (run '(eof-object? (void))) #f))

  (begin ;; Extort
    (check-equal? (run '(add1 #f)) 'err)
    (check-equal? (run '(sub1 #f)) 'err)
    (check-equal? (run '(zero? #f)) 'err)
    (check-equal? (run '(char->integer #f)) 'err)
    (check-equal? (run '(integer->char #f)) 'err)
    (check-equal? (run '(integer->char -1)) 'err)
    (check-equal? (run '(write-byte #f)) 'err)
    (check-equal? (run '(write-byte -1)) 'err)
    (check-equal? (run '(write-byte 256)) 'err)
    (check-equal? (run '(begin (integer->char 97)
                               (integer->char 98)))
                  #\b))

  (begin ;; Fraud
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
    (check-equal? (run '(let ((x (+ 1 2)))
                          (let ((z (- 4 x)))
                            (+ (+ x x) z))))
                  7)

    (check-equal? (run '(= 5 5)) #t)
    (check-equal? (run '(= 4 5)) #f)
    (check-equal? (run '(= (add1 4) 5)) #t)
    (check-equal? (run '(< 5 5)) #f)
    (check-equal? (run '(< 4 5)) #t)
    (check-equal? (run '(< (add1 4) 5)) #f)))

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
    (check-equal? (run "†" '(peek-byte)) (cons 226 "")))

  (begin ;; Extort
    (check-equal? (run "" '(write-byte #t)) (cons 'err "")))

  (begin ;; Fraud
    (check-equal? (run "" '(let ((x 97)) (write-byte x))) (cons (void) "a"))
    (check-equal? (run ""
                       '(let ((x 97))
                          (begin (write-byte x)
                                 x)))
                  (cons 97 "a"))
    (check-equal? (run "b" '(let ((x 97)) (begin (read-byte) x)))
                  (cons 97 ""))
    (check-equal? (run "b" '(let ((x 97)) (begin (peek-byte) x)))
                  (cons 97 ""))))

