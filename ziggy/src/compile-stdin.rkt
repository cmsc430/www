#lang crook
{:= A B C D0 D1 E0 E1 F H0 H1}
(provide main)
(require "parse.rkt")
(require "compile.rkt")
(require a86/printer)

;; -> Void
;; Compile contents of stdin,
;; emit asm code on stdout
(define (main)
  (read-line) ; ignore #lang racket line
  (asm-display (compile (parse (read)))))
