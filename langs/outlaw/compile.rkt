#lang racket
(provide (all-defined-out))
(require "ast.rkt"
         "types.rkt"
         "lambdas.rkt"
         "fv.rkt"
         "utils.rkt"
         "compile-define.rkt"
         "compile-expr.rkt"
         "compile-literals.rkt"
         a86/ast)

;; Registers used
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; type CEnv = [Listof Id]

(define (compile p)
  (match p
    [(Prog ds e)
     (let ((gs (append stdlib-ids (define-ids ds))))
       (seq (externs)
            (map (lambda (i) (Extern (symbol->label i))) stdlib-ids)
            (Global 'entry)
            (Label 'entry)
            (Mov rbx rdi) ; recv heap pointer
            (init-symbol-table p)
            (init-lib)
            
            (compile-defines ds gs)
            (compile-e e '() gs #t)
            (Ret)
            (compile-lambda-defines (lambdas p) gs)
            (Global 'raise_error_align)
            (Label 'raise_error_align)
            pad-stack
            (Mov rdi 0) ; null arg
            (Call 'raise_error)

          ;; one way to make `cons' a function instead of a primitive
          ;;cons-function
          
          (Data)
          (compile-literals p)))]))

(define (init-lib)
  (let ((r (gensym))) ; call init_lib
    (seq (Extern 'init_lib)
         (Lea rax r)
         (Push rax)
         (Jmp 'init_lib)
         (Label r))))

(define stdlib-ids
  '(list make-list list? foldr map length append
         memq member append-map vector->list
         reverse
         number->string gensym read read-char
         > <= >=
         void?
         list->string string->list
         char<=?
         remove-duplicates remq* remove* remove
         ;; Op0
         read-byte peek-byte void
         ;; Op1
         add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer
         box unbox box? empty? cons? car cdr
         vector? vector-length string? string-length
         symbol->string string->symbol symbol?
         string->uninterned-symbol
         open-input-file
         write-char error integer?
         eq-hash-code
         ;; Op2
         + - < = cons eq? make-vector vector-ref
         make-string string-ref string-append
         quotient remainder set-box!
         bitwise-and bitwise-ior bitwise-xor arithmetic-shift         
         ;; Op3
         vector-set!))

(define (externs)
  (map Extern
       '(peek_byte
         read_byte
         write_byte
         raise_error
         intern_symbol
         symb_cmp
         string_append
         memcpy
         open_input_file
         read_byte_port
         peek_byte_port)))

(define cons-function
  (let ((code (gensym 'cons_code))
        (clos (gensym 'cons_closure)))
    (seq (Data)
         (Label (symbol->label 'cons))
         (Dq (Plus (symbol->label clos) type-proc))
         (Label (symbol->label clos))
         (Dq (symbol->label code))
         (Text)
         (Label (symbol->label code))
         (Pop rax)
         (Mov (Offset rbx 0) rax)
         (Pop rax)
         (Mov (Offset rbx 8) rax)
         (Add rsp 8) ; pop function
         (Mov rax rbx)
         (Or rax type-cons)
         (Add rbx 16)
         (Ret))))


;; Lib -> Asm
(define (compile-library l)
  (match l
    [(Lib ids ds)
     (let ((g (define-ids ds)))
       (seq (externs)
            (map (lambda (i) (Global (symbol->label i))) ids)
            (Extern 'raise_error_align)
            
            (Global 'init_lib)
            (Label 'init_lib)
            (compile-defines ds g)
            (Ret)
            
            (compile-lambda-defines (lambdas-ds ds) g)
            (Data)
            (compile-literals (Prog ds (Quote #t)))))]))
