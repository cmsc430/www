#lang racket
(provide (all-defined-out))
(require "printer.rkt" ffi/unsafe)

;; Asm -> Integer
;; Interpret (by assemblying, linking, and loading) x86-64 code
;; Assume: entry point is "entry"
(define (asm-interp a)
  (define t.s  (make-temporary-file "nasm~a.s"))
  (define t.o  (path-replace-extension t.s #".o"))
  (define t.so (path-replace-extension t.s #".so"))
  (with-output-to-file t.s
    #:exists 'truncate
    (λ ()
      (displayln (asm-string a))))

  (system (format "nasm -f ~a ~a && gcc -shared ~a -o ~a"
                  (if (eq? (system-type 'os) 'macosx) 'macho64 'elf64)
                  t.s t.o t.so))

  (define libt.so (ffi-lib t.so))
  (delete-file t.s)
  (delete-file t.o)
  (delete-file t.so)
  ((get-ffi-obj "entry" libt.so (_fun -> _int64))))
