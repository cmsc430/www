#lang crook
{:= A B C D0 D1 E0 E1 F H0 H1}
(provide main)
(require "parse.rkt")
(require "compile.rkt")
(require "run.rkt")

;; -> Void
;; Compile contents of stdin and use asm-interp to run
(define (main)
  (read-line) ; ignore #lang racket line
  (run (compile (parse (read)))))
