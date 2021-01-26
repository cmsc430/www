#lang racket
(provide/contract
 [asm-string (-> (listof instruction?) string?)])

(define current-shared?
  (make-parameter #f))

(module* private #f
  (provide current-shared?))

(require "ast.rkt")

;; Arg -> String
(define (arg->string a)
  (match a
    [(? reg?) (reg->string a)]
    [(? integer?) (number->string a)]
    [(Offset (? reg? r) i)
     (string-append "[" (reg->string r) " + " (number->string i) "]")]))

;; Any -> Boolean
(define (reg? x)
  (register? x))

;; Reg -> String
(define (reg->string r)
  (symbol->string r))

;; Asm -> String
(define (asm-string a)
  (define external-labels '())

  ;; Label -> String
  ;; prefix with _ for Mac
  (define label-symbol->string
    (match (system-type 'os)
      ['macosx
       (λ (s) (string-append "_" (symbol->string s)))]
      [_
       (if (current-shared?)
           (λ (s)
                  (if (memq s external-labels)
                      ; hack for ELF64 shared libraries in service of
                      ; calling external functions in asm-interp
                      (string-append (symbol->string s) " wrt ..plt")
                      (symbol->string s)))
           symbol->string)]))

  ;; (U Label Reg) -> String
  (define (jump-target->string t)
    (match t
      [(? reg?) (reg->string t)]
      [_ (label-symbol->string t)]))

  ;; Instruction -> String
  (define (instr->string i)
    (match i
      [(Ret)       "\tret\n"]
      [(Label l)   (string-append (label-symbol->string l) ":\n")]
      [(Extern l)  (begin0 (string-append "\textern " (label-symbol->string l) "\n")
                           (set! external-labels (cons l external-labels)))]
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
                      (arg->string a2) "\n")]    
      [(Cmp a1 a2)
       (string-append "\tcmp "
                      (arg->string a1) ", "
                      (arg->string a2) "\n")]
      [(Sal a1 a2)
       (string-append "\tsal "
                      (arg->string a1) ", "
                      (arg->string a2) "\n")]
      [(Sar a1 a2)
       (string-append "\tsar "
                      (arg->string a1) ", "
                      (arg->string a2) "\n")]
      [(And a1 a2)
       (string-append "\tand "
                      (arg->string a1) ", "
                      (arg->string a2) "\n")]
      [(Or a1 a2)
       (string-append "\tor "
                      (arg->string a1) ", "
                      (arg->string a2) "\n")]    
      [(Xor a1 a2)
       (string-append "\txor "
                      (arg->string a1) ", "
                      (arg->string a2) "\n")]
      [(Jmp l)
       (string-append "\tjmp "
                      (jump-target->string l) "\n")]
      [(Je l)
       (string-append "\tje "
                      (jump-target->string l) "\n")]
      [(Jne l)
       (string-append "\tjne "
                      (jump-target->string l) "\n")]
      [(Jl l)
       (string-append "\tjl "
                      (jump-target->string l) "\n")]
      [(Jg l)
       (string-append "\tjg "
                      (jump-target->string l) "\n")]
      [(Call l)
       (string-append "\tcall "
                      (jump-target->string l) "\n")]
      [(Push a)
       (string-append "\tpush "
                      (arg->string a) "\n")]
      [(Pop r)
       (string-append "\tpop "
                      (reg->string r) "\n")]
      [(Lea d x)
       (string-append "\tlea "
                      (arg->string d) ", [rel "
                      (label-symbol->string x) "]\n")]))

  ;; entry point will be first label
  (match (findf Label? a)
    [(Label g)
     (string-append
      "\tglobal " (label-symbol->string g) "\n"
      "\tdefault rel\n"
      "\tsection .text\n"
      (foldl (λ (i s) (string-append s (instr->string i))) "" a))]
    [_
     (error "program does not have an initial label")]))
