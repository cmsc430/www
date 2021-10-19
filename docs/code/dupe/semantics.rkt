#lang racket
(provide D-concrete D 𝑫 𝑫-𝒑𝒓𝒊𝒎 is-true is-false)
(require redex/reduction-semantics
         (only-in "../con/semantics.rkt" C C-concrete))

(define-extended-language D-concrete C-concrete
  (e ::= .... boolean (if e e e) (zero? e)))
  
(define-extended-language D C
  ; new defn to get rid of IfZero
  (e ::= (Int i) (Bool b) (Prim1 p1 e) (If e e e))
  (p1 ::= .... 'zero?)
  (v ::= i b)
  (b ::= #t #f))

(define-judgment-form D
  #:mode (𝑫 I O)
  #:contract (𝑫 e v)  
  [--------
   (𝑫 (Int i) i)]

  [--------
   (𝑫 (Bool b) b)]

  [(𝑫 e_0 v_0) (where v_1 (𝑫-𝒑𝒓𝒊𝒎 p1 v_0))
   -----------
   (𝑫 (Prim1 p1 e_0) v_1)]

  [(𝑫 e_0 v_0) (is-true v_0) (𝑫 e_1 v_1)
   --------
   (𝑫 (If e_0 e_1 e_2) v_1)]
  
  [(𝑫 e_0 v_0) (is-false v_0) (𝑫 e_2 v_2)
   --------
   (𝑫 (If e_0 e_1 e_2) v_2)])

(define-metafunction D
  𝑫-𝒑𝒓𝒊𝒎 : p1 v -> v or ⊥
  [(𝑫-𝒑𝒓𝒊𝒎 'add1 i) ,(+ (term i) (term 1))]
  [(𝑫-𝒑𝒓𝒊𝒎 'sub1 i) ,(- (term i) (term 1))]
  [(𝑫-𝒑𝒓𝒊𝒎 'zero? 0) #t]
  [(𝑫-𝒑𝒓𝒊𝒎 'zero? i) #f]
  [(𝑫-𝒑𝒓𝒊𝒎 _ _) ⊥])

(module+ test
  (test-judgment-holds (𝑫 (Int 7) 7))
  (test-judgment-holds (𝑫 (Bool #f) #f))
  (test-judgment-holds (𝑫 (Bool #t) #t))
  (test-judgment-holds (𝑫 (Prim1 'add1 (Int 8)) 9))
  (test-judgment-holds (𝑫 (Prim1 'sub1 (Int 8)) 7))

  (test-judgment-holds (𝑫 (If (Bool #f) (Int 3) (Int 4)) 4))
  (test-judgment-holds (𝑫 (If (Bool #t) (Int 3) (Int 4)) 3))
  (test-judgment-holds (𝑫 (Prim1 'zero? (Int 0)) #t))
  (test-judgment-holds (𝑫 (Prim1 'zero? (Int 1)) #f))
  (test-judgment-holds (𝑫 (If (Prim1 'zero? (Int 0)) (Int 3) (Int 4)) 3))
  (test-judgment-holds (𝑫 (If (Prim1 'zero? (Int 1)) (Int 3) (Int 4)) 4)))

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
