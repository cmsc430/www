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

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (init-symbol-table p)
           (compile-defines-values ds)
           (compile-e e (reverse (define-ids ds)) #t)
           (Add rsp (* 8 (length ds))) ;; pop function definitions
           (Ret)
           (compile-defines ds)
           (compile-lambda-defines (lambdas p))
           (Label 'raise_error_align)
           pad-stack
           (Call 'raise_error)

           ; cons function
           (let ((c (gensym 'cons)))
             (seq (Data)
                  (Label (symbol->label 'cons))
                  (Dq (symbol->label c))
                  (Text)
                  (Label (symbol->label c))
                  (Pop rax)
                  (Mov (Offset rbx 0) rax)
                  (Pop rax)
                  (Mov (Offset rbx 8) rax)
                  (Add rsp 8) ; pop function
                  (Mov rax rbx)
                  (Or rax type-cons)
                  (Add rbx 16)
                  (Ret)))  
           
             
           (Data)
           (compile-literals p))]))

(define (externs)
  (seq (Extern 'peek_byte)
       (Extern 'read_byte)
       (Extern 'write_byte)
       (Extern 'raise_error)
       (Extern 'intern_symbol)
       (Extern 'symb_cmp)
       (Extern 'memcpy)))
