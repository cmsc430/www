#lang racket
(provide (all-defined-out))
(require "compile.rkt" "asm/printer.rkt" "parse.rkt")

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (writeln "Compiling..." (current-error-port))
      (let ((c (read-line)) ; ignore #lang racket line
            (p (read)))
        (asm-display (compile (parse p)))))))
