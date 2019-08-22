#lang racket
(provide D 𝑫)
(require redex/reduction-semantics
         (only-in "../con/semantics.rkt" C))

(define-extended-language D C
  (e ::= .... v (if e e e) (zero? e))
  (v ::= i #t #f))

(define-judgment-form D
  #:mode (𝑫 I O)
  #:contract (𝑫 e v)  
  [--------
   (𝑫 v v)]

  [(𝑫 e_0 i_0) (where i_1 ,(+ (term i_0) 1))
   -----------
   (𝑫 (add1 e_0) i_1)]

  [(𝑫 e_0 i_0) (where i_1 ,(- (term i_0) 1))
   -----------
   (𝑫 (sub1 e_0) i_1)]

  [(𝑫 e_0 i) (where v ,(= (term i) 0))
   -----------
   (𝑫 (zero? e_0) v)]

  [(𝑫 e_0 v_0) (where #f ,(eqv? (term v_0) #f)) (𝑫 e_1 v_1)
   --------
   (𝑫 (if e_0 e_1 e_2) v_1)]
  
  [(𝑫 e_0 #f) (𝑫 e_2 v_2)
   --------
   (𝑫 (if e_0 e_1 e_2) v_2)])

(define (!= n1 n2)
  (not (= n1 n2)))

(module+ test
  (test-judgment-holds (𝑫 7 7))
  (test-judgment-holds (𝑫 #f #f))
  (test-judgment-holds (𝑫 #t #t))
  (test-judgment-holds (𝑫 (add1 8) 9))
  (test-judgment-holds (𝑫 (sub1 8) 7))

  (test-judgment-holds (𝑫 (if #f 3 4) 4))
  (test-judgment-holds (𝑫 (if #t 3 4) 3))
  (test-judgment-holds (𝑫 (zero? 0) #t))
  (test-judgment-holds (𝑫 (zero? 1) #f))
  (test-judgment-holds (𝑫 (if (zero? 0) 3 4) 3))
  (test-judgment-holds (𝑫 (if (zero? 1) 3 4) 4)))