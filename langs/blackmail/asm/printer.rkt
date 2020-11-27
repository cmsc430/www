#lang racket
(provide asm-string)
(require "ast.rkt")

;; Instruction -> String
(define (instr->string i)
  (match i
    [(Ret)       "\tret\n"]
    [(Label l)   (string-append (label-symbol->string l) ":\n")]
    [(Mov a1 a2)
     (string-append "\tmov "
                    (arg->string a1) ", "
                    (arg->string a2) "\n")]
    [(Add a1 a2)
     (string-append "\tadd "
                    (arg->string a1) ", "
                    (arg->string a2) "\n")]
    [(Sub a1 a2)
     (string-append "\tsub "
                    (arg->string a1) ", "
                    (arg->string a2) "\n")]))
    

;; Arg -> String
(define (arg->string a)
  (match a
    ['rax "rax"]
    [n (number->string n)]))

;; Symbol -> String
;; prefix with _ for Mac
(define label-symbol->string
  (match (system-type 'os)
    ['macosx
     (λ (s) (string-append "_" (symbol->string s)))]
    [_ symbol->string]))

;; Asm -> String
(define (asm-string a)
  ;; entry point will be first label
  (match (findf Label? a)
    [(Label g)
     (string-append
      "\tglobal " (label-symbol->string g) "\n"
      "\tsection .text\n"
      (foldr (λ (i s) (string-append (instr->string i) s)) "" a))]))
