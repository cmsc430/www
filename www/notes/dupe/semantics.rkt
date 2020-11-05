#lang racket
(provide D-concrete D 𝑫 is-true is-false)
(require redex/reduction-semantics
         (only-in "../con/semantics.rkt" C C-concrete))

(define-extended-language D-concrete C-concrete
  (e ::= .... boolean (if e e e) (zero? e)))
  
(define-extended-language D C
  (e ::= .... (Bool b) (If e e e) (Zero? e))
  (v ::= i b)
  (b ::= #t #f))

(define-judgment-form D
  #:mode (𝑫 I O)
  #:contract (𝑫 e v)  
  [--------
   (𝑫 (Int i) i)]

  [--------
   (𝑫 (Bool b) b)]

  [(𝑫 e_0 i_0) (where i_1 ,(+ (term i_0) 1))
   -----------
   (𝑫 (Add1 e_0) i_1)]

  [(𝑫 e_0 i_0) (where i_1 ,(- (term i_0) 1))
   -----------
   (𝑫 (Sub1 e_0) i_1)]

  [(𝑫 e_0 i) (side-condition ,(= (term i) 0))
   -----------
   (𝑫 (Zero? e_0) #t)]

  [(𝑫 e_0 i) (side-condition ,(!= (term i) 0))
   -----------
   (𝑫 (Zero? e_0) #f)]

  [(𝑫 e_0 v_0) (is-true v_0) (𝑫 e_1 v_1)
   --------
   (𝑫 (If e_0 e_1 e_2) v_1)]
  
  [(𝑫 e_0 v_0) (is-false v_0) (𝑫 e_2 v_2)
   --------
   (𝑫 (If e_0 e_1 e_2) v_2)])

(module+ test
  (test-judgment-holds (𝑫 (Int 7) 7))
  (test-judgment-holds (𝑫 (Bool #f) #f))
  (test-judgment-holds (𝑫 (Bool #t) #t))
  (test-judgment-holds (𝑫 (Add1 (Int 8)) 9))
  (test-judgment-holds (𝑫 (Sub1 (Int 8)) 7))

  (test-judgment-holds (𝑫 (If (Bool #f) (Int 3) (Int 4)) 4))
  (test-judgment-holds (𝑫 (If (Bool #t) (Int 3) (Int 4)) 3))
  (test-judgment-holds (𝑫 (Zero? (Int 0)) #t))
  (test-judgment-holds (𝑫 (Zero? (Int 1)) #f))
  (test-judgment-holds (𝑫 (If (Zero? (Int 0)) (Int 3) (Int 4)) 3))
  (test-judgment-holds (𝑫 (If (Zero? (Int 1)) (Int 3) (Int 4)) 4)))

(define-judgment-form D
  #:mode (is-true I)
  #:contract (is-true v)
  [-----------
   (is-true #t)]
  [----------
   (is-true i)])

(define-judgment-form D
  #:mode (is-false I)
  #:contract (is-false v)
  [-----------
   (is-false #f)])

(define (!= n m)
  (not (= n m)))
