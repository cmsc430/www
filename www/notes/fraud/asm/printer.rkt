#lang racket
(provide (all-defined-out))

;; Asm -> String
(define (asm->string a)
  (foldr (λ (i s) (string-append (instr->string i) s)) "" a))

;; Instruction -> String
(define (instr->string i)
  (match i
    [`(mov ,a1 ,a2)
     (string-append "\tmov " (arg->string a1) ", " (arg->string a2) "\n")]
    [`(add ,a1 ,a2)
     (string-append "\tadd " (arg->string a1) ", " (arg->string a2) "\n")]
    [`(sub ,a1 ,a2)
     (string-append "\tsub " (arg->string a1) ", " (arg->string a2) "\n")]
    [`(cmp ,a1 ,a2)
     (string-append "\tcmp " (arg->string a1) ", " (arg->string a2) "\n")]
    [`(jmp ,l)
     (string-append "\tjmp " (label->string l) "\n")]
    [`(je ,l)
     (string-append "\tje " (label->string l) "\n")]
    [`(jne ,l)
     (string-append "\tjne " (label->string l) "\n")]
    [`ret "\tret\n"]
    [l (string-append (label->string l) ":\n")]))

;; Arg -> String
(define (arg->string a)
  (match a
    [(? reg?) (reg->string a)]
    [`(offset ,r ,i)
     (string-append "[" (reg->string r) " + " (number->string (* i 8)) "]")]
    [(? integer?) (number->string a)]))

;; Any -> Boolean
(define (reg? x)
  (and (symbol? x)
       (memq x '(rax rsp))))

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
      		     "\tsection .text\n"
                     (asm->string a)))))
