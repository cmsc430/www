#lang racket
(provide G 𝑮)
(require redex/reduction-semantics
         (only-in "../fraud/semantics.rkt" F 𝑭𝒓))

(define-extended-language G F
  (e ::= .... (p1 e) (p2 e e))
  (p2 ::= + -)
  (p1 ::= add1 sub1)
  (p ::= p1 p2))

(define-judgment-form G
  #:contract (𝑮 e a)
  #:mode (𝑮 I O)
  [(𝑮𝒓 e () a)
   ----------
   (𝑮 e a)])

(define-extended-judgment-form G 𝑭𝒓
  #:contract (𝑮𝒓 e r a)
  #:mode (𝑮𝒓 I I O)
  
  [(𝑮𝒓 e_0 r a_0) ... (𝑷𝒓𝒊𝒎 (p a_0 ...) a_1)
   -----------
   (𝑮𝒓 (p e_0 ...) r a_1)])

(define-judgment-form G
  #:contract (𝑷𝒓𝒊𝒎 (p a ...) a)
  #:mode (𝑷𝒓𝒊𝒎 I O)

  [(where i_1 ,(add1 (term i_0)))
   ---------------
   (𝑷𝒓𝒊𝒎 (add1 i_0 ) i_1)]

  [(where i_1 ,(sub1 (term i_0)))
   ---------------
   (𝑷𝒓𝒊𝒎 (sub1 i_0 ) i_1)]

  [(where i_2 ,(+ (term i_0) (term i_1)))
   ---------------
   (𝑷𝒓𝒊𝒎 (+ i_0 i_1) i_2)]

  [(where i_2 ,(- (term i_0) (term i_1)))
   ---------------
   (𝑷𝒓𝒊𝒎 (- i_0 i_1) i_2)]
  
  [---------------
   (𝑷𝒓𝒊𝒎 (p _ ... err _ ...) err)]  

  [---------------
   (𝑷𝒓𝒊𝒎 (p _ ... b _ ...) err)])

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
