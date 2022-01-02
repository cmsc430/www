#lang racket
(provide main)

;; This is a utility for smashing together racket files into a single
;; monolithic program.

;; For example:

;; racket -t combine.rkt -m compile-stdin.rkt stdlib.rkt > outlaw.rkt

;; creates a file with all the source code needed for the Outlaw
;; compiler.

;; It will append the source code of all the files fn requires
;; and comment out their requires and provides, excluding
;; any files given in the ignores list.

;; String String ... -> Void
;; Combine all the files fn depends upon, print to stdout
;; as one monolithic program, excluding ignores.
(define (main fn . ignores)
  (printf "#lang racket\n")
  (let ((fs (remove* ignores (all-files fn))))
    (for-each (lambda (f)
                (displayln (string-append ";; " f)))
              fs)
    (print-files fs)))

;; Port -> [Listof S-Expr]
;; read all s-expression until eof
(define (read-all p)
  (let ((r (read p)))
    (if (eof-object? r)
        '()
        (cons r (read-all p)))))

(define (print-files fs)
  (match fs
    ['() (void)]
    [(cons f fs)
     (displayln (make-string 72 #\;))
     (displayln (string-append ";; " f "\n"))
     (print-file f)
     (print-files fs)]))

(define (print-file f)
  (let ((p (open-input-file f)))
    (read-line p) ; ignore #lang
    (define (loop)
      (let ((l (read-line p)))
        (if (eof-object? l)
            (begin (newline)
                   (close-input-port p))
            (begin
              (when (regexp-match? #rx"^\\(require|^\\(provide" l)
                (display "#;"))
              (displayln l)
              (loop)))))
    (loop)))

(define (all-files fn)
  (remove-duplicates (all-files* fn '())))

(define (all-files* fn seen)
  (if (member fn seen)
      '()
      (let ((p (open-input-file fn)))
        (read-line p) ; ignore #lang
        (begin0
          (let ((rs (get-requires (read-all p))))
            (append (append-map (λ (f) (all-files* f (cons fn seen))) rs)
                    (list fn)))         
          (close-input-port p)))))      

(define (get-requires s)
  (match s
    ['() '()]
    [(cons (cons 'require rs) s)
     (append (filter string? rs) (get-requires s))]
    [(cons _ s)
     (get-requires s)]))
    

