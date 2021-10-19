#lang racket
(provide B-concrete B 𝑩)
(require redex/reduction-semantics)

(define-language B-concrete
  (e ::= integer (add1 e) (sub1 e)))

(define-language B
  (e ::= (Int i) (Prim1 p1 e))  
  (i ::= integer)
  (p1 ::= 'add1 'sub1))

(define-judgment-form B
  #:mode (𝑩 I O)
  #:contract (𝑩 e i)  
  [----------
   (𝑩 (Int i) i)]

  [(𝑩 e_0 i_0) (where i_1 ,(+ (term i_0) 1))
   -----------
   (𝑩 (Prim1 'add1 e_0) i_1)]
  
  [(𝑩 e_0 i_0) (where i_1 ,(- (term i_0) 1))
   -----------
   (𝑩 (Prim1 'sub1 e_0) i_1)])

(module+ test
  (test-judgment-holds (𝑩 (Int 7) 7))
  (test-judgment-holds (𝑩 (Prim1 'add1 (Int 8)) 9))
  (test-judgment-holds (𝑩 (Prim1 'sub1 (Int 8)) 7)))
