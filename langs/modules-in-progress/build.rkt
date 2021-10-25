#lang racket
(provide main)
(require "parse.rkt" "ast.rkt" (rename-in "compile-file.rkt" [main compile-file]))

;; Compiles a complete program, including dependencies

(define (fmt)
  (match (system-type 'os)
    ['unix "elf64"]
    ['macosx "macho64"]))
  
(define (main fn)
  (let ((root (suffix fn ""))
        (ds (apply string-append (add-between (deps fn) " "))))
   ; (printf "%.s: %.rkt\n\tracket -t compile-file.rkt -m $< > $@\n\n")
   ; (printf ".s.o:\n\tnasm -g -f ~a -o $@ $<\n\n" (fmt))
    (printf "~arun: ~ao runtime.o ~a\n\tgcc ~a $< runtime.o -o $@\n\n" root root ds ds)))
            
            
(define (suffix fn s)
  (string-append (substring fn 0 (- (string-length fn) 3)) s))

(define (deps fn)
  (match (parse-module-file fn)
    [(Module ps rs ds)
     (append-map (λ (r) (cons (suffix (car r) "o") (deps (car r)))) rs)]))
