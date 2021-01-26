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

(define fopen
  (get-ffi-obj "fopen" (ffi-lib #f) (_fun _path _string/utf-8 _-> _pointer)))

(define fflush
  (get-ffi-obj "fflush" (ffi-lib #f) (_fun _pointer _-> _void)))

(define fclose
  (get-ffi-obj "fclose" (ffi-lib #f) (_fun _pointer _-> _void)))

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
   (format "nasm -f ~a ~a && gcc -shared ~a ~a -o ~a"
           (if (eq? (system-type 'os) 'macosx) 'macho64 'elf64)
           t.s
           t.o
           (string-splice (current-objs))
           t.so))

  (define libt.so (ffi-lib t.so))

  (define init-label
    (match (findf Label? a)
      [(Label l) l]
      [_ (error "no initial label found")]))

  (define entry
    (get-ffi-obj init-label libt.so (_fun _-> _int64)))

  ;; install our own `error_handler` procedure to prevent `exit` calls
  ;; from interpreted code bringing down the parent process.  All of
  ;; these hooks into the runtime need a better API and documentation,
  ;; but this is a rough hack to make Extort work for now.
  (when (ffi-obj-ref "error_handler" libt.so (thunk #f))
    (set-ffi-obj! "error_handler" libt.so _pointer
                  (function-ptr (λ () (raise 'err)) (_fun _-> _void))))
  
  (delete-file t.s)
  (delete-file t.o)
  (delete-file t.so)
  (if input
      (let ()
        (unless (and (ffi-obj-ref "in" libt.so (thunk #f))
                     (ffi-obj-ref "out" libt.so (thunk #f)))
          (error "asm-interp/io: running in IO mode without IO linkage"))

        (with-output-to-file t.in #:exists 'truncate
          (thunk (display input)))

        (define current-in
          (make-c-parameter "in" libt.so _pointer))
        (define current-out
          (make-c-parameter "out" libt.so _pointer))

        (current-in  (fopen t.in "r"))
        (current-out (fopen t.out "w"))

        (define result
          (with-handlers ((symbol? identity))
            (entry)))

        (fflush (current-out))
        (fclose (current-in))
        (fclose (current-out))

        (define output (file->string t.out))
        (delete-file t.in)
        (delete-file t.out)
        (cons result output))
      (with-handlers ((symbol? identity))
        (entry))))


(define (string-splice xs)
  (apply string-append (add-between xs " ")))
