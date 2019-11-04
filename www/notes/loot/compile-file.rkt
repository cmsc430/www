#lang racket
(provide (all-defined-out))
(require "compile.rkt" #;"syntax.rkt" "asm/printer.rkt")

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((p (read-program)))
        ; assumed OK for now
        ;(unless (and (prog? p) (closed? p))
        ;  (error "syntax error"))          
        (asm-display (compile p))))))

(define (read-program)
  (regexp-match "^#lang racket" (current-input-port))
  (read))
