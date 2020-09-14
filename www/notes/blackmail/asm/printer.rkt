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
    [`(call ,ad)
     (string-append "\tcall " (label->string ad) "\n")]
    [`(and ,a1 ,a2)
     (string-append "\tand " (arg->string a1) ", " (arg->string a2) "\n")]
    [`ret "\tret\n"]
    [l (string-append (label->string l) ":\n")]))

;; Arg -> String
(define (arg->string a)
  (match a
    [`rax "rax"]
    [`rsp "rsp"]
    [`r15 "r15"]
    [n (number->string n)]))

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
      		     "\textern get_int\n"
      		     "\tsection .text\n"
                     (asm->string a)))))
