#lang racket
(provide E-concrete E 𝑬)
(require redex/reduction-semantics
         (only-in "../dupe/semantics.rkt" D-concrete D 𝑫))

(define-extended-language E-concrete D-concrete
  (e ::= ....)
  (a ::= v err))

(define-extended-language E D
  (e ::= ....)
  (a ::= v err))

(define-extended-judgment-form E 𝑫
  #:mode (𝑬 I O)
  #:contract (𝑬 e a)
  [(𝑬 e b)
   --------
   (𝑬 (Prim1 'add1 e) err)]

  [(𝑬 e b)
   -----------
   (𝑬 (Prim1 'sub1 e) err)]

  [(𝑬 e b)
   -----------
   (𝑬 (Prim1 'zero? e) err)]

  [(𝑬 e err)
   -----------
   (𝑬 (Prim1 'zero? e) err)]

  [(𝑬 e err)
   -----------
   (𝑬 (Prim1 'add1 e) err)]

  [(𝑬 e err)
   -----------
   (𝑬 (Prim1 'sub1 e) err)]

  [(𝑬 e err)
   -----------
   (𝑬 (If e e_0 e_1) err)])


(module+ test
  (test-judgment-holds (𝑬 (Int 7) 7))
  (test-judgment-holds (𝑬 (Bool #f) #f))
  (test-judgment-holds (𝑬 (Bool #t) #t))
  (test-judgment-holds (𝑬 (Prim1 'add1 (Int 8)) 9))
  (test-judgment-holds (𝑬 (Prim1 'sub1 (Int 8)) 7))

  (test-judgment-holds (𝑬 (If (Bool #f) (Int 3) (Int 4)) 4))
  (test-judgment-holds (𝑬 (If (Bool #t) (Int 3) (Int 4)) 3))
  (test-judgment-holds (𝑬 (Prim1 'zero? (Int 0)) #t))
  (test-judgment-holds (𝑬 (Prim1 'zero? (Int 1)) #f))
  (test-judgment-holds (𝑬 (If (Prim1 'zero? (Int 0)) (Int 3) (Int 4)) 3))
  (test-judgment-holds (𝑬 (If (Prim1 'zero? (Int 1)) (Int 3) (Int 4)) 4))

  
  (test-judgment-holds (𝑬 (Prim1 'add1 (Bool #t)) err))
  (test-judgment-holds (𝑬 (Prim1 'add1 (Bool #f)) err))
  (test-judgment-holds (𝑬 (Prim1 'sub1 (Bool #t)) err))
  (test-judgment-holds (𝑬 (Prim1 'sub1 (Bool #f)) err))
  (test-judgment-holds (𝑬 (Prim1 'zero? (Bool #t)) err))
  (test-judgment-holds (𝑬 (Prim1 'zero? (Bool #f)) err))

  (test-judgment-holds (𝑬 (Prim1 'add1 (If (Bool #t) (Bool #t) (Bool #t))) err))
  (test-judgment-holds (𝑬 (Prim1 'sub1 (If (Bool #t) (Bool #t) (Bool #t))) err))
  (test-judgment-holds (𝑬 (Prim1 'zero? (If (Bool #t) (Bool #t) (Bool #t))) err))
  
  (test-judgment-holds (𝑬 (Prim1 'add1 (Prim1 'zero? (Bool #f))) err))
  (test-judgment-holds (𝑬 (Prim1 'sub1 (Prim1 'zero? (Bool #f))) err))
  (test-judgment-holds (𝑬 (Prim1 'zero? (Prim1 'zero? (Bool #f))) err))
  (test-judgment-holds (𝑬 (If (Prim1 'zero? (Bool #f)) (Int 1) (Int 2)) err)))
