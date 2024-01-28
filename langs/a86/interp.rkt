#lang racket
(provide/contract
 [current-objs  (parameter/c (listof path-string?))]
 [asm-interp    (-> (listof instruction?) any/c)]
 [asm-interp/io (-> (listof instruction?) string? any/c)])

(define-logger a86)

(require "printer.rkt" "ast.rkt" "callback.rkt" "check-nasm.rkt"
         (rename-in ffi/unsafe [-> _->]))
(require (submod "printer.rkt" private))

;; Check NASM availability when required to fail fast.
(check-nasm-available)

(define *debug*?
  (let ((r (getenv "PLTSTDERR")))
    (and r
         (string=? r "info@a86"))))

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

(define fmt (if (eq? (system-type 'os) 'macosx) 'macho64 'elf64))

;; WARNING: The heap is re-used, so make sure you're done with it
;; before calling asm-interp again
(define *heap*
  ; IMPROVE ME: hard-coded heap size
  (malloc _int64 20000 'raw))


;; Integer64 -> String
(define (int64->binary-string n)
  (format "#b~a"
          (~r n #:base 2 #:min-width 64 #:pad-string "0")))

;; Integer64 -> String
(define (int64->octal-string n)
  (format "#o~a"
          (~r n #:base 8 #:min-width 22 #:pad-string "0")))

;; Integer64
(define (int64->hex-string n)
  (format "#x~a"
          (~r n #:base 16 #:min-width 16 #:pad-string "0")))

(define (show-state . regs)
  (format "\n~a"
          (map (lambda (r v)
                 (format "(~a ~a)" r (int64->hex-string v)))
               '(rax rbx rcx rdx rbp rsp rsi rdi
                     r8 r9 r10 r11 r12 r13 r14 r15 instr flags)
               regs)))

;; Asm String -> (cons Value String)
;; Like asm-interp, but uses given string for input and returns
;; result with string output
(define (asm-interp/io a input)

  (log-a86-info (~v a))

  (define t.s   (make-temporary-file "nasm~a.s"))
  (define t.o   (path-replace-extension t.s #".o"))
  (define t.so  (path-replace-extension t.s #".so"))
  (define t.in  (path-replace-extension t.s #".in"))
  (define t.out (path-replace-extension t.s #".out"))

  (with-output-to-file t.s
    #:exists 'truncate
    (λ ()
      (parameterize ((current-shared? #t))
        (asm-display (if *debug*?
                         (debug-transform a)
                         a)))))

  (nasm t.s t.o)
  (ld t.o t.so)

  (define libt.so (ffi-lib t.so))

  (define init-label
    (match (findf Label? a)
      [(Label l) l]
      [_ (error "no initial label found")]))

  (define entry
    (get-ffi-obj init-label libt.so (_fun _pointer _-> _int64)))

  ;; install our own `error_handler` procedure to prevent `exit` calls
  ;; from interpreted code bringing down the parent process.  All of
  ;; these hooks into the runtime need a better API and documentation,
  ;; but this is a rough hack to make Extort work for now.
  (when (ffi-obj-ref "error_handler" libt.so (thunk #f))
    (set-ffi-obj! "error_handler" libt.so _pointer
                  (function-ptr (λ () (raise 'err)) (_fun _-> _void))))

  (when *debug*?
    (define log (ffi-obj-ref log-label libt.so (thunk #f)))
    (when log
      (set-ffi-obj! log-label libt.so _pointer
                    (function-ptr
                     (λ () (log-a86-info
                            (apply show-state
                                   (build-list 18 (lambda (i) (ptr-ref log _int64 (add1 i)))))))
                     (_fun _-> _void)))))

  (define has-heap? #f)

  (when (ffi-obj-ref "heap" libt.so (thunk #f))
    (set! has-heap? #t)

    ;; This is a GC-enabled run-time so set from, to, and types space
    (when (ffi-obj-ref "from" libt.so (thunk #f))
      ;; FIXME: leaks types memory
      (set-ffi-obj! "from" libt.so _pointer *heap*)
      (set-ffi-obj! "to" libt.so _pointer (ptr-add *heap* 10000 _int64))
      (set-ffi-obj! "types" libt.so _pointer (malloc _int32 10000))))

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
            (guard-foreign-escape
             (entry *heap*))))

        (fflush (current-out))
        (fclose (current-in))
        (fclose (current-out))

        (define output (file->string t.out))
        (delete-file t.in)
        (delete-file t.out)
        (cons result output))

      (with-handlers ((symbol? identity))
        (guard-foreign-escape
         (entry *heap*)))))


(define (string-splice xs)
  (apply string-append
         (add-between (map (lambda (s) (string-append "\"" s "\"")) xs)
                      " ")))

;;; Utilities for calling nasm and linker with informative error messages

(struct exn:nasm exn:fail:user ())
(define nasm-msg
  (string-append
   "assembly error: make sure to use `prog` to construct an assembly program\n"
   "if you did and still get this error; please share with course staff."))

(define (nasm:error msg)
  (raise (exn:nasm (format "~a\n\n~a" nasm-msg msg)
                   (current-continuation-marks))))

;; run nasm on t.s to create t.o
(define (nasm t.s t.o)
  (define err-port (open-output-string))
  (unless (parameterize ((current-error-port err-port))
            (system (format "nasm -f ~a ~a -o ~a" fmt t.s t.o)))
    (nasm:error (get-output-string err-port))))

(struct exn:ld exn:fail:user ())
(define (ld:error msg)
  (raise (exn:ld (format "link error: ~a" msg)
                 (current-continuation-marks))))

(define (ld:undef-symbol s)
  (ld:error
   (string-append
    (format "symbol ~a not defined in linked objects: ~a\n" s (current-objs))
    "use `current-objs` to link in object containing symbol definition.")))

;; link together t.o with current-objs to create shared t.so
(define (ld t.o t.so)
  (define err-port (open-output-string))
  (define objs (string-splice (current-objs)))
  (define -z-defs-maybe
    (if (eq? (system-type 'os) 'macosx)
        ""
        "-z defs "))
  (unless (parameterize ((current-error-port err-port))
            (system (format "gcc ~a-v -shared ~a ~a -o ~a"
                            -z-defs-maybe
                            t.o objs t.so)))
    (define err-msg
      (get-output-string err-port))
    (match (or (regexp-match #rx"Undefined.*\"(.*)\"" err-msg)            ; mac
               (regexp-match #rx"undefined reference to `(.*)'" err-msg)) ; linux
      [(list _ symbol) (ld:undef-symbol symbol)]
      [_ (ld:error (format "unknown link error.\n\n~a" err-msg))])))



;; Debugging facilities

(define log-label (symbol->label (gensym 'log)))

(define (Log i)
  (seq (save-registers)
       (Pushf)
       (Mov 'rax i)
       (Mov (Offset log-label (* 8 17)) 'rax)
       (Mov 'rax (Offset 'rsp 0))
       (Mov (Offset log-label (* 8 18)) 'rax)
       (Call (Offset log-label 0))
       (Popf)
       (restore-registers)))

(define (instrument is)
  (for/fold ([ls '()]
             #:result (reverse ls))
            ([idx (in-naturals)]
             [ins (in-list is)])
    (if (serious-instruction? ins)
        (seq ins (reverse (Log idx)) ls)
        (seq ins ls))))

(define (serious-instruction? ins)
  (match ins
    [(Label _) #f]
    [(Global _) #f]
    [(? Comment?) #f]
    [_ #t]))

(define (debug-transform is)
  (seq (instrument is)
          ;; End of user program
          (Data)
          (Global log-label)
          (Label log-label)
          (Dq 0) ; callback placeholder
          (static-alloc-registers)
          (Dq 0) ; index of instruction
          (Dq 0) ; flags
          ))

(define registers
  '(rax rbx rcx rdx rbp rsp rsi rdi
        r8 r9 r10 r11 r12 r13 r14 r15))

(define (static-alloc-registers)
  (apply seq
         (map (λ (r) (seq (Dq 0) (% (~a r))))
              registers)))

(define (save-registers)
  (apply seq
         (map (λ (r i) (seq (Mov (Offset log-label (* 8 i)) r)))
              registers
              (build-list (length registers) add1))))

(define (restore-registers)
  (apply seq
         (map (λ (r i) (seq (Mov r (Offset log-label (* 8 i)))))
              registers
              (build-list (length registers) add1))))
