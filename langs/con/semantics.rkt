#lang racket
(provide C-concrete C 𝑪)
(require redex/reduction-semantics
         (only-in "../blackmail/semantics.rkt" B B-concrete 𝑩))

(define-extended-language C-concrete B-concrete
  (e ::= .... (if (zero? e) e e)))

(define-extended-language C B
  (e ::= .... (IfZero e e e)))

(define-extended-judgment-form C 𝑩
  #:mode (𝑪 I O)
  #:contract (𝑪 e i)
  [(𝑪 e_0 i_0) (side-condition ,(= (term i_0) 0)) (𝑪 e_1 i_1)
   --------
   (𝑪 (IfZero e_0 e_1 e_2) i_1)]
  
  [(𝑪 e_0 i_0) (side-condition ,(!= (term i_0) 0)) (𝑪 e_2 i_2)
   --------
   (𝑪 (IfZero e_0 e_1 e_2) i_2)])

(define (!= n1 n2)
  (not (= n1 n2)))

(module+ test
  (test-judgment-holds (𝑪 (Int 7) 7))
  (test-judgment-holds (𝑪 (Prim1 'add1 (Int 8)) 9))
  (test-judgment-holds (𝑪 (Prim1 'sub1 (Int 8)) 7))
  (test-judgment-holds (𝑪 (IfZero (Prim1 'sub1 (Int 1))
                                  (Int 3)
                                  (Int 4))
                          3))
  (test-judgment-holds (𝑪 (IfZero (Prim1 'add1 (Int 1))
                                  (Int 3)
                                  (Int 4))
                          4)))
