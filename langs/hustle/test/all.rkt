#lang racket
(require "../compile.rkt"
         "../interp.rkt"
         "../interp-io.rkt"         
         "../parse.rkt"
         "../types.rkt"
         "../unload-bits-asm.rkt"
         a86
         rackunit)

;; link with runtime for IO operations
(unless (file-exists? "../runtime.o")
  (system "make -C .. runtime.o"))
(current-objs
 (list (path->string (normalize-path "../runtime.o"))))

(define (test-runner run)
  
  ;; Abscond examples
  (check-equal? (run 7) 7)
  (check-equal? (run -8) -8)

  ;; Blackmail examples
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

  ;; Dupe examples
  (check-equal? (run #t) #t)
  (check-equal? (run #f) #f)
  (check-equal? (run (if #t 1 2)) 1)
  (check-equal? (run (if #f 1 2)) 2)
  (check-equal? (run (if 0 1 2)) 1)
  (check-equal? (run '(if #t 3 4)) 3)
  (check-equal? (run '(if #f 3 4)) 4)
  (check-equal? (run '(if  0 3 4)) 3)
  (check-equal? (run '(zero? 4)) #f)
  (check-equal? (run '(zero? 0)) #t)
  ;; Dodger examples
  (check-equal? (run #\a) #\a)
  (check-equal? (run #\b) #\b)
  (check-equal? (run '(char? #\a)) #t)
  (check-equal? (run '(char? #t)) #f)
  (check-equal? (run '(char? 8)) #f)
  (check-equal? (run '(char->integer #\a)) (char->integer #\a))
  (check-equal? (run '(integer->char 955)) #\λ)
  ;; Extort examples
  (check-equal? (run '(add1 #f)) 'err)
  (check-equal? (run '(sub1 #f)) 'err)
  (check-equal? (run '(zero? #f)) 'err)
  (check-equal? (run '(char->integer #f)) 'err)
  (check-equal? (run '(integer->char #f)) 'err)
  (check-equal? (run '(integer->char -1)) 'err)
  (check-equal? (run '(write-byte #f)) 'err)
  (check-equal? (run '(write-byte -1)) 'err)
  (check-equal? (run '(write-byte 256)) 'err)
  ;; Fraud examples
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

  ;; Hustle examples
  (check-equal? (run '(unbox (box 7))) 7)
  (check-equal? (run '(let ((x (box 2))) (unbox x))) 2)
  (check-equal? (run '(let ((x (cons 2 '()))) (car x))) 2))

(test-runner (λ (e) (interp (parse e))))
(test-runner (λ (e) (unload/free (asm-interp (compile (parse e))))))

(define (test-runner-io run)
  ;; Evildoer examples
  (check-equal? (run 7 "") (cons 7 ""))
  (check-equal? (run '(write-byte 97) "") (cons (void) "a"))
  (check-equal? (run '(read-byte) "a") (cons 97 ""))
  (check-equal? (run '(begin (write-byte 97) (read-byte)) "b")
                (cons 98 "a"))
  (check-equal? (run '(read-byte) "") (cons eof ""))
  (check-equal? (run '(eof-object? (read-byte)) "") (cons #t ""))
  (check-equal? (run '(eof-object? (read-byte)) "a") (cons #f ""))
  (check-equal? (run '(begin (write-byte 97) (write-byte 98)) "")
                (cons (void) "ab"))

  (check-equal? (run '(peek-byte) "ab") (cons 97 ""))
  (check-equal? (run '(begin (peek-byte) (read-byte)) "ab") (cons 97 ""))
  ;; Extort examples
  (check-equal? (run '(write-byte #t) "") (cons 'err ""))

  ;; Fraud examples
  (check-equal? (run '(let ((x 97)) (write-byte x)) "") (cons (void) "a"))
  (check-equal? (run '(let ((x 97))
                        (begin (write-byte x)
                               x))
                     "")
                (cons 97 "a"))
  (check-equal? (run '(let ((x 97)) (begin (read-byte) x)) "b")
                (cons 97 ""))
  (check-equal? (run '(let ((x 97)) (begin (peek-byte) x)) "b")
                (cons 97 "")))
  

(test-runner-io (λ (e s) (interp/io (parse e) s)))
(test-runner-io (λ (e s)
                  (match (asm-interp/io (compile (parse e)) s)
                    [(cons 'err o) (cons 'err o)]
                    [(cons r o) (cons (unload/free r) o)])))

;; run command line compiler and compare against Racket as refernece implementation
(require rackunit "../../test-programs/get-progs.rkt")
(for-each test-prog (get-progs "hustle"))
