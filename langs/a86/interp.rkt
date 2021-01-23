#lang racket
(provide/contract
 [current-objs  (parameter/c (listof path-string?))]
 [asm-interp    (-> (listof instruction?) any/c)]
 [asm-interp/io (-> (listof instruction?) string? any/c)])

(require "printer.rkt" "ast.rkt" (rename-in ffi/unsafe [-> _->]))
(require (submod "printer.rkt" private))

;; Assembly code is linked with object files in this parameter
(define current-objs
  (make-parameter '()))

;; Asm -> Value
;; Interpret (by assemblying, linking, and loading) x86-64 code
;; Assume: entry point is "entry"
(define (asm-interp a)
  (asm-interp/io a #f))

;; Asm String -> (cons Value String)
;; Like asm-interp, but uses given string for input and returns
;; result with string output
(define (asm-interp/io a input)
  (define t.s   (make-temporary-file "nasm~a.s"))
  (define t.o   (path-replace-extension t.s #".o"))
  (define t.so  (path-replace-extension t.s #".so"))
  (define t.in  (path-replace-extension t.s #".in"))
  (define t.out (path-replace-extension t.s #".out"))

  (with-output-to-file t.s
    #:exists 'truncate
    (λ ()
      (parameterize ((current-shared? #t))
        (displayln (asm-string a)))))

  (system
   (format "nasm -f ~a ~a && gcc -fPIC -shared ~a ~a -o ~a"
           (if (eq? (system-type 'os) 'macosx) 'macho64 'elf64)
           t.s
           t.o
           (string-splice (current-objs))
           t.so))

  (define libt.so (ffi-lib t.so))
  (define entry (get-ffi-obj "entry" libt.so (_fun _-> _int64)))
  (delete-file t.s)
  (delete-file t.o)
  (delete-file t.so)
  (if input
      (let ()
        (define set-io!
          (get-ffi-obj "set_io" libt.so (_fun _path _path _-> _void)))
        (define close-io!
          (get-ffi-obj "close_io" libt.so (_fun _-> _void)))
        (with-output-to-file t.in #:exists 'truncate
          (thunk (display input)))
        (set-io! t.in t.out)
        (define result (entry))
        (close-io!)
        (define output (file->string t.out))
        (delete-file t.in)
        (delete-file t.out)
        (cons result output))
      (entry)))


(define (string-splice xs)
  (apply string-append (add-between xs " ")))
