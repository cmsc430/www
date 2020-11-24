#lang racket
(provide (all-defined-out))
(require "printer.rkt" racket/runtime-path)
(define-runtime-path dir "..")

;; Asm -> Value
;; Interpret (by assemblying, linking, and exec'ing) x86-64 code
;; Assume: starts with entry point run-time expects
(define (asm-interp a) 
  (with-input-from-string (asm-interp/io a "") read))

;; Asm String -> String
;; Like asm-interp but pipes given input to exec'd program
;; and collects output as a string...
(define (asm-interp/io a in)
  (let* ((t.s  (make-temporary-file "nasm~a.s"))
         (t.in (make-temporary-file "in~a.txt"))
         (t.run (path-replace-extension t.s #".run")))
    (with-output-to-file t.in
      #:exists 'truncate
      (λ ()
        (display in)))   
    (with-output-to-file t.s
      #:exists 'truncate
      (λ ()
        (displayln (asm-string a))))
    (system (format "(cd ~a && make -s ~a) 2>&1 >/dev/null" dir t.run))
    (delete-file t.s)
    (with-output-to-string
      (λ ()
        (system (string-append "cat " (path->string t.in)
                               " | " (path->string t.run)))
        (delete-file t.run)))))     
  