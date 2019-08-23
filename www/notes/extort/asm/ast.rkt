#lang racket

;; type Asm = [Listof Instruction]
 
;; type Instruction =
;; | `ret
;; | `(mov ,Arg ,Arg)
;; | `(add ,Arg ,Arg)
;; | `(sub ,Arg ,Arg)
;; | `(cmp ,Arg ,Arg)
;; | `(mul ,Arg ,Arg)
;; | `(sub ,Arg ,Arg)
;; | `(jmp ,Label)
;; | `(je  ,Label)
;; | `(jne ,Label)
;; | `(sete ,Arg)
;; | `(movzbl ,Arg ,Arg)
;; | `(sal ,Arg ,Arg)
;; | `(or ,Arg ,Arg)
;; | `(call ,Label)
;; | Label
 
;; type Label = Symbol
 
;; type Arg =
;; | Reg
;; | Integer
;; | `(offset ,Reg ,Integer)
 
;; type Reg =
;; | `rax
;; | `rsp
