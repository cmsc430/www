#lang racket
(provide G 𝑮 𝑮-r 𝑮-prim 𝑮-type-error)
(require redex/reduction-semantics
         (only-in "../fraud/semantics.rkt" F 𝑭𝒓))

(define-extended-language G F
  (e ::= .... (p1 e) (p2 e e))
  (p2 ::= + -)
  (p1 ::= add1 sub1 zero?)
  (p ::= p1 p2))

(define-judgment-form G
  #:contract (𝑮 e a)
  #:mode (𝑮 I O)
  [(𝑮-r e () a)
   ----------
   (𝑮 e a)])

(define-extended-judgment-form G 𝑭𝒓
  #:contract (𝑮-r e r a)
  #:mode (𝑮-r I I O)
  
  [(𝑮-r e_0 r a_0) ... (𝑮-prim (p a_0 ...) a_1)
   ----------- prim
   (𝑮-r (p e_0 ...) r a_1)])

(define-judgment-form G
  #:contract (𝑮-prim (p a ...) a)
  #:mode (𝑮-prim I O)

  [(where i_1 ,(add1 (term i_0)))
   --------------- add1
   (𝑮-prim (add1 i_0 ) i_1)]

  [(where i_1 ,(sub1 (term i_0)))
   --------------- sub1
   (𝑮-prim (sub1 i_0 ) i_1)]

  [(where i_2 ,(+ (term i_0) (term i_1)))
   --------------- +
   (𝑮-prim (+ i_0 i_1) i_2)]

  [(where i_2 ,(- (term i_0) (term i_1)))
   --------------- minus
   (𝑮-prim (- i_0 i_1) i_2)]
  
  [--------------- prop-error
   (𝑮-prim (p v ... err _ ...) err)]  

  [(𝑮-type-error (p v ...))
   --------------- type-error
   (𝑮-prim (p v ...) err)])

(define-judgment-form G
  ;; Commented out to allow extension (since its buggy in redex)
  ;; #:contract (𝑮-type-error (p v ...))
  #:mode (𝑮-type-error I )
  [(𝑮-type-error (+ b _))]
  [(𝑮-type-error (+ _ b))]
  [(𝑮-type-error (- b _))]
  [(𝑮-type-error (- _ b))]
  [(𝑮-type-error (add1 b))]
  [(𝑮-type-error (sub1 b))])

(module+ test
  (test-judgment-holds (𝑮 7 7))
  (test-judgment-holds (𝑮 (add1 7) 8))

  (test-judgment-holds (𝑮 (add1 #f) err))
  
  (test-judgment-holds (𝑮 (let ((x 7)) 8) 8))
  (test-judgment-holds (𝑮 (let ((x 7)) x) 7))
  (test-judgment-holds (𝑮 (let ((x 7)) (add1 x)) 8))
  (test-judgment-holds (𝑮 (sub1 (let ((x 7)) (add1 x))) 7))
  (test-judgment-holds (𝑮 (sub1 (let ((x 7))
                                  (let ((y x))
                                    (add1 x))))
                          7))
  (test-judgment-holds (𝑮 (sub1 (let ((x 7))
                                  (let ((x 8))
                                    (add1 x))))
                          8))

  (test-judgment-holds (𝑮 (+ 1 2) 3))
  (test-judgment-holds (𝑮 (- 1 2) -1))
  (test-judgment-holds (𝑮 (add1 #f) err))
  (test-judgment-holds (𝑮 (if (add1 #f) 1 2) err))
  (test-judgment-holds (𝑮 (+ 1 (add1 #f)) err))
  (test-judgment-holds (𝑮 (+ 1 #f) err))
  (test-judgment-holds (𝑮 (- 1 #f) err))
  (test-judgment-holds (𝑮 (- (add1 #f) #f) err)))
