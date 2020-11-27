#lang racket
(provide B-concrete B 𝑩)
(require redex/reduction-semantics)

(define-language B-concrete
  (e ::= integer (add1 e) (sub1 e)))

(define-language B
  (e ::= (Int i) (Add1 e) (Sub1 e))
  (i ::= integer))

(define-judgment-form B
  #:mode (𝑩 I O)
  #:contract (𝑩 e i)  
  [----------
   (𝑩 (Int i) i)]

  [(𝑩 e_0 i_0) (where i_1 ,(+ (term i_0) 1))
   -----------
   (𝑩 (Add1 e_0) i_1)]

  [(𝑩 e_0 i_0) (where i_1 ,(- (term i_0) 1))
   -----------
   (𝑩 (Sub1 e_0) i_1)])

  
