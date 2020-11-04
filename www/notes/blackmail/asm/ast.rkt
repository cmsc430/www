#lang racket
(provide (all-defined-out))

;; type Asm = [Listof Instruction]
;; type Instruction =
;; | (Label Symbol)
;; | (Ret)
;; | (Mov Arg Arg)
;; | (Add Arg Arg)
;; | (Sub Arg Arg)
;; type Arg =
;; | 'rax
;; | Number
(struct Label (x) #:prefab)
(struct Ret () #:prefab)
(struct Mov (a1 a2) #:prefab)
(struct Add (a1 a2) #:prefab)
(struct Sub (a1 a2) #:prefab)
