#lang racket
(provide (all-defined-out))
(require "asm-printer.rkt")

;; Asm -> Integer
;; Interpret (by assemblying, linking, and exec'ing) x86-64 code
;; Assume: starts with entry point run-time expects
(define (interp-asm a)
  (let* ((t.s (make-temporary-file "nasm~a.s"))
         (t.run (path-replace-extension t.s #".run")))
    (with-output-to-file t.s
      #:exists 'truncate
      (λ ()
        (display-asm a)))
    (system (format "make -s ~a" t.run))
    (delete-file t.s)
    (with-input-from-string
        (with-output-to-string
          (λ ()
            (system (path->string t.run))
            (delete-file t.run)))
      read)))
