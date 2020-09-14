#lang racket
(provide (all-defined-out))
(require "compile.rkt" "syntax.rkt" "asm/printer.rkt" "parse.rkt")

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((c (read-line))) ; ignore #lang racket
        (let ((p (read)))
          (asm-display (compile (parse p))))))))
