#lang racket
(provide B 𝑩)
(require redex/reduction-semantics)

(define-language B
  (e ::= i (add1 e) (sub1 e))
  (i ::= integer))

(define-judgment-form B
  #:mode (𝑩 I O)
  #:contract (𝑩 e i)  
  [----------
   (𝑩 i i)]

  [(𝑩 e_0 i_0) (where i_1 ,(+ (term i_0) 1))
   -----------
   (𝑩 (add1 e_0) i_1)]

  [(𝑩 e_0 i_0) (where i_1 ,(- (term i_0) 1))
   -----------
   (𝑩 (sub1 e_0) i_1)])

  
