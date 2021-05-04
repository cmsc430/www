#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Background ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; All the information here is based on the System-V x86_64 ABI. Specifically
;; Section 3.2 of the AMD64 Architecture Supplement. A copy can be found here:
;;
;; https://refspecs.linuxfoundation.org/elf/x86_64-abi-0.99.pdf
;;
;; The following webpage also provides a 'cheat sheet' for most of the relevant
;; aspects for our use-case: https://wiki.osdev.org/Calling_Conventions



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Parameter registers
;;
;; According the the System-V x86_64 ABI integer-like parameters should be
;; passed in the following registers, _in the following order_.
;;
;; So the first integer-like argument to a function should be passed in 'rdi
;; the second in 'rsi, etc. etc.
(define arg-regs '(rdi rsi rdx rcx r8 r9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Callee Save Registers
;;
;; The following registers must be saved by the _callee_ if the callee needs to
;; use them. Because of this, the caller does not need to worry about preserving
;; their values before a function call.
(define callee-saves '(rbp rbx r12 r13 r14 r15))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Caller Save Registers
;;
;; The following registers must be saved by the _caller_, if the caller will
;; need their value after the function is executed.
(define caller-saves '(rcx rdx rdi rsi r8 r9 r10 r11))
