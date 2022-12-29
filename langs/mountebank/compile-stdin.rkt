#lang racket
(provide main)
(require "parse.rkt" "compile.rkt" "read-all.rkt" a86/printer)

;; -> Void
;; Compile contents of stdin,
;; emit asm code on stdout
(define (main)
  (read-line) ; ignore #lang racket line
  (asm-display (compile (parse (read-all)))))
