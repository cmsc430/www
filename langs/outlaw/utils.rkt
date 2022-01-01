#lang racket
(provide symbol->label symbol->data-label lookup pad-stack unpad-stack *2 *8 *10 *16 *64)
(require "a86/ast.rkt" "registers.rkt")

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (to-label "label_" s))

(define (symbol->data-label s)
  (to-label "data_" s))

(define (to-label prefix s)
  (string->symbol
   (string-append
    prefix
    (apply string-append
           (map (λ (c)
                  (if (or (char<=? #\a c #\z)
                          (char<=? #\A c #\Z)
                          (char<=? #\0 c #\9)
                          (memq c '(#\_ #;#\$ #\# #\@ #\~ #\. #\?)))
                      (make-string 1 c)
                      (string-append "$" (number->string (char->integer c) 16))))
                (string->list (symbol->string s)))))))

;; Id CEnv -> [Maybe Integer]
(define (lookup x cenv)
  (match cenv
    ['() #f]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (match (lookup x rest)
             [#f #f]
             [i (+ 8 i)])])]))

;; -> Asm
;; Dynamically pad the stack to be aligned for a call
(define (pad-stack)
  (seq (Mov r15 rsp)
       (And r15 #b1000)
       (Sub rsp r15)))

;; -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack)
  (seq (Add rsp r15)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multipliers

(define (*2 a)
   (arithmetic-shift a 1))

(define (*8 a)
   (arithmetic-shift a 3))

(define (*16 a)
   (arithmetic-shift a 4))

(define (*10 a) ; 10a=2^3a+2a
  (+ (arithmetic-shift a 1)
     (arithmetic-shift a 3)))

(define (*64 a)
  (arithmetic-shift a 6))
