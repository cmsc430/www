#lang racket
(provide (all-defined-out))
(require "printer.rkt" racket/runtime-path)
(define-runtime-path dir "..")

;; Asm -> Integer
;; Interpret (by assemblying, linking, and exec'ing) x86-64 code
;; Assume: starts with entry point run-time expects
(define (asm-interp a)
  (let* ((t.s (make-temporary-file "nasm~a.s"))
         (t.run (path-replace-extension t.s #".run")))
    (with-output-to-file t.s
      #:exists 'truncate
      (λ ()
        (asm-display a)))
    (with-output-to-string ; ignore make output
      (λ () 
        (system (format "(cd ~a && make -s ~a)" dir t.run))))
    (delete-file t.s)
    (with-input-from-string
        (with-output-to-string
          (λ ()
            (system (path->string t.run))
            (delete-file t.run)))
      read)))
