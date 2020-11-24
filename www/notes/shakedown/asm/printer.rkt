#lang racket
(provide (all-defined-out))

;; Asm -> String
(define (asm->string a)
  (foldr (λ (i s) (string-append (instr->string i) s)) "" a))

;; Instruction -> String
(define (instr->string i)
  (match i
    [`(,(? opcode2? o) ,a1 ,a2)
     (string-append "\t"
                    (symbol->string o) " "
                    (arg->string a1) ", "
                    (arg->string a2) "\n")]    
    [`(jmp ,l)
     (string-append "\tjmp " (arg->string l) "\n")]
    [`(je ,l)
     (string-append "\tje " (label->string l) "\n")]
    [`(jle ,l)
     (string-append "\tjle " (label->string l) "\n")]
    [`(jl ,l)
     (string-append "\tjl " (label->string l) "\n")]
    [`(jg ,l)
     (string-append "\tjg " (label->string l) "\n")]
    [`(jge ,l)
     (string-append "\tjge " (label->string l) "\n")]
    [`(jne ,l)
     (string-append "\tjne " (label->string l) "\n")]
    [`ret "\tret\n"]
    [`(neg ,a1)
     (string-append "\tneg " (arg->string a1) "\n")]
    [`(call ,l)
     (string-append "\tcall " (arg->string l) "\n")]
    [`(push ,r)
     (string-append "\tpush " (reg->string r) "\n")]    
    [`(extern ,f)
     (string-append "\textern " (label->string f) "\n")]
    [`(section text) "\tsection .text\n"]
    [l (string-append (label->string l) ":\n")]))

(define (opcode2? x)
  (memq x '(mov add sub cmp and cmovl xor or sal sar lea)))

;; Arg -> String
(define (arg->string a)
  (match a
    [(? reg?) (reg->string a)]
    [`(offset ,r)
     (string-append "[" (arg->string r) "]")]
    [`(offset ,r ,i)
     (string-append "[" (arg->string r) " + " (number->string (* i 8)) "]")]    
    [(? integer?) (number->string a)]
    [(? symbol?) (label->string a)]))

;; Any -> Boolean
(define (reg? x)
  (and (symbol? x)
       (memq x '(rax rbx rcx rdx rsp rdi rip rbp rsi r8 r9 r10 r11 r12 r13 r14 r15))))

;; Reg -> String
(define (reg->string r)
  (symbol->string r))

;; Label -> String
;; prefix with _ for Mac
(define label->string
  (match (system-type 'os)
    ['macosx
     (λ (s) (string-append "_" (symbol->string s)))]
    [_ symbol->string]))

;; Asm -> Void
(define (asm-display a)
  ;; entry point will be first label
  (let ((g (findf symbol? a)))
    (display 
     (string-append "\tglobal " (label->string g) "\n"
                    "\tdefault rel\n"
                    "\textern " (label->string 'error) "\n"                    
                    (asm->string a)))))
