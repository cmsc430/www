#lang racket
(provide (all-defined-out))
(require "compile.rkt" "syntax.rkt" "ast.rkt")
(require "dot.rkt" "pretty-printer.rkt" "render-ast.rkt" "restricted.rkt")

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((c (read-line))
            (p (read)))
        (unless (expr? p) (error "syntax error" p))
        (display (seq->string (ppr-graph (render-prog (sexpr->prog p) "prog"))))))))
