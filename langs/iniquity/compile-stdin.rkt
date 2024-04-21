#lang racket
(provide main)
(require "parse.rkt")
(require "compile.rkt")
(require "read-all.rkt")
(require a86/printer)

;; -> Void
;; Compile contents of stdin,
;; emit asm code on stdout
(define (main)
  (read-line) ; ignore #lang racket line
  (asm-display (compile (apply parse (read-all)))))

