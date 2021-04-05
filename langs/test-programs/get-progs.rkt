#lang racket
(provide get-progs test-prog)
(require racket/runtime-path rackunit)

(define-runtime-path here ".")

(define ordered-langs
  '("abscond"
    "blackmail"
    "con"
    "dupe"
    "dodger"
    "evildoer"
    "extort"
    "fraud"
    "hustle"
    "iniquity"
    "jig"))

;; String -> (listof Path)
(define (get-progs lang)
  (append-map (lambda (l) (find-files (lambda (p) (path-has-extension? p #".rkt"))
                                      (normalize-path (build-path here l))))
              (langs-before lang ordered-langs)))

;; String -> (Listof String)
;; Get a list of all the languages that come "before" given one
(define (langs-before lang ols)
  (match ols
    ['() '()]
    [(cons l ols)
     (if (string=? lang l)
         (list l)
         (cons l (langs-before lang ols)))]))

(module+ test
  (check-equal? (langs-before "abscond" ordered-langs)
                '("abscond"))
  (check-equal? (langs-before "blackmail" ordered-langs)
                '("abscond" "blackmail")))

(define (test-prog p.rkt)
  (define p.run (path-replace-extension p.rkt ".run"))
  (define p.in  (path-replace-extension p.rkt ".in"))
  (when (file-exists? p.run)
    (delete-file p.run))
  (check-true (make p.run))
  (if (file-exists? p.in)
      (check-equal? (run/io p.run p.in)
                    (racket/io p.rkt p.in))
      (check-equal? (run p.run)
                    (racket p.rkt))))

(define (system/s cmd)
  (with-output-to-string (thunk (system cmd))))

;; these are a little kludgey and could be done better

(define (racket p)
  (parameterize ((current-error-port (open-output-string)))
    (let ((r (system/s (string-append "racket " (path->string p)))))
      (if (string=? "" (get-output-string (current-error-port)))
          r
          (string-append r "err\n")))))

(define (racket/io p in)
  (parameterize ((current-error-port (open-output-string)))
    (let ((r (system/s (string-append "cat " (path->string in) " | racket " (path->string p)))))
      (if (string=? "" (get-output-string (current-error-port)))
          r
          (string-append r "err\n")))))

(define (run p)
  (system/s (path->string p)))

(define (run/io p in)
  (system/s (string-append "cat " (path->string in) " | " (path->string p))))

(define (make p)
  (system (string-append "make -C .. -s " (path->string p))))

