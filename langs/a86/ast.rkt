#lang racket
(provide (all-defined-out))

;; type Asm = [Listof Instruction]
;; type Instruction =
;; | (Label Symbol)
;; | (Cal Symbol)
;; | (Ret)
;; | (Mov Arg Arg)
;; | (Add Arg Arg)
;; | (Sub Arg Arg)
;; | (Cmp Arg Arg)
;; | (Jmp Symbol)
;; | (Je  Symbol)
;; | (Jne Symbol)
;; | (And Arg Arg)
;; | (Or Arg Arg)
;; | (Xor Arg Arg)
;; | (Sal Arg Arg)
;; | (Sar Arg Arg)
;; | (Push Arg)
;; | (Pop Arg)
;; type Arg =
;; | 'rax
;; | Number
;; | (Offset Reg Integer)
(struct Label (x)     #:prefab)
(struct Call  (x)     #:prefab)
(struct Ret   ()      #:prefab)
(struct Mov   (a1 a2) #:prefab)
(struct Add   (a1 a2) #:prefab)
(struct Sub   (a1 a2) #:prefab)
(struct Cmp   (a1 a2) #:prefab)
(struct Jmp   (x)     #:prefab)
(struct Je    (x)     #:prefab)
(struct Jne   (x)     #:prefab)
(struct Jl    (x)     #:prefab)
(struct Jg    (x)     #:prefab)
(struct And   (a1 a2) #:prefab)
(struct Or    (a1 a2) #:prefab)
(struct Xor   (a1 a2) #:prefab)
(struct Sal   (a1 a2) #:prefab)
(struct Sar   (a1 a2) #:prefab)
(struct Push  (a1)    #:prefab)
(struct Pop   (a1)    #:prefab)

(struct Offset (r i)  #:prefab)

;; (U Instruction Asm) ... -> Asm
;; Convenient for sequencing instructions or groups of instructions
(define (seq . xs)
  (foldr (λ (x is)
           (if (list? x)
               (append x is)
               (cons x is)))
         '()
         xs))
