#lang racket
(provide G 𝑮 𝑮-𝒆𝒏𝒗 𝑮-𝒑𝒓𝒊𝒎)
(require redex/reduction-semantics
         (only-in "../fraud/semantics.rkt" F))

(define-extended-language G F
  (e ::= .... (p1 e) (p2 e e))
  (p2 ::= + -)
  (p1 ::= add1 sub1 zero?)
  (p ::= p1 p2))

(define-judgment-form G
  #:contract (𝑮 e a)
  #:mode (𝑮 I O)
  [(𝑮-𝒆𝒏𝒗 e () a)
   ----------
   (𝑮 e a)])

(define-judgment-form G
  #:contract (𝑮-𝒆𝒏𝒗 e r a)
  #:mode (𝑮-𝒆𝒏𝒗 I I O)

  ;; Value
  [-----------
   (𝑮-𝒆𝒏𝒗 v r v)]

  ;; If
  [(𝑮-𝒆𝒏𝒗 e_0 r v_0) (side-condition (is-true v_0)) (𝑮-𝒆𝒏𝒗 e_1 r a)
   --------
   (𝑮-𝒆𝒏𝒗 (if e_0 e_1 e_2) r a)]

  [(𝑮-𝒆𝒏𝒗 e_0 r v_0) (side-condition (is-false v_0)) (𝑮-𝒆𝒏𝒗 e_2 r a)
   --------
   (𝑮-𝒆𝒏𝒗 (if e_0 e_1 e_2) r a)]

  [(𝑮-𝒆𝒏𝒗 e_0 r err)
   --------
   (𝑮-𝒆𝒏𝒗 (if e_0 e_1 e_2) r err)]

  ;; Let and variable
  [(where a (lookup r x))
   -----------
   (𝑮-𝒆𝒏𝒗 x r a)]

  [(𝑮-𝒆𝒏𝒗 e_0 r v_0) (𝑮-𝒆𝒏𝒗 e_1 (ext r x v_0) a)
   -----
   (𝑮-𝒆𝒏𝒗 (let ((x e_0)) e_1) r a)]

  [(𝑮-𝒆𝒏𝒗 e_0 r err)
   -----------
   (𝑮-𝒆𝒏𝒗 (let ((x e_0)) e_1) r err)]

  ;; Primitive application
  [(𝑮-𝒆𝒏𝒗 e_0 r a_0) ...
   -----------
   (𝑮-𝒆𝒏𝒗 (p e_0 ...) r (𝑮-𝒑𝒓𝒊𝒎 (p a_0 ...)))])

(define-metafunction G
  𝑮-𝒑𝒓𝒊𝒎 : (p a ...) -> a
  [(𝑮-𝒑𝒓𝒊𝒎 (p v ... err _ ...)) err]
  [(𝑮-𝒑𝒓𝒊𝒎 (add1 i_0)) ,(+ (term i_0) 1)]
  [(𝑮-𝒑𝒓𝒊𝒎 (sub1 i_0)) ,(- (term i_0) 1)]
  [(𝑮-𝒑𝒓𝒊𝒎 (zero? 0)) #t]
  [(𝑮-𝒑𝒓𝒊𝒎 (zero? i)) #f]
  [(𝑮-𝒑𝒓𝒊𝒎 (+ i_0 i_1)) ,(+ (term i_0) (term i_1))]
  [(𝑮-𝒑𝒓𝒊𝒎 (- i_0 i_1)) ,(- (term i_0) (term i_1))]
  [(𝑮-𝒑𝒓𝒊𝒎 _) err])

(define-metafunction G
  ext : r x v -> r
  [(ext ((x_0 v_0) ...) x v)
   ((x v) (x_0 v_0) ...)])

(define-metafunction G
  lookup : r x -> a
  [(lookup () x) err]
  [(lookup ((x v) (x_1 v_1) ...) x) v]
  [(lookup ((x_0 v_0) (x_1 v_1) ...) x)
   (lookup ((x_1 v_1) ...) x)])

(define-metafunction G
  is-true : v -> boolean
  [(is-true #f) #f]
  [(is-true v)  #t])

(define-metafunction G
  is-false : v -> boolean
  [(is-false #f) #t]
  [(is-false v)  #f])

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

(module+ test
  (require rackunit)
  ;; Check that the semantics is total function
  (redex-check G e
               (check-true (redex-match? G (a_0) (judgment-holds (𝑮 e a) a)))
               #:print? #f))
