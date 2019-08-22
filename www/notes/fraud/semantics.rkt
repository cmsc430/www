#lang racket
(provide D D-pre 𝑫 𝑫𝒓 lookup ext)
(require redex/reduction-semantics)

; for use in presentations (informally noting x can't be let, etc.)
(define-language D-pre
  (e ::= i x (add1 e) (sub1 e) (let ((x e)) e))
  (x ::= variable)
  (i ::= integer))

;; the real grammar language
(define-extended-language D D-pre
  (x ::= variable-not-otherwise-mentioned)
  (r ::= ((x i) ...)))

(module+ test
  (test-equal (redex-match? D e (term x)) #t)
  (test-equal (redex-match? D e (term let)) #f)
  (test-equal (redex-match? D e (term (let ((x 1)) x))) #t)
  (test-equal (redex-match? D e (term (let ((let 1)) 3))) #f))

(module+ test
  (test-equal (redex-match? D-pre e (term x)) #t)
  (test-equal (redex-match? D-pre e (term let)) #t)
  (test-equal (redex-match? D-pre e (term (let ((x 1)) x))) #t)
  (test-equal (redex-match? D-pre e (term (let ((let 1)) 3))) #t))

(define-judgment-form D
  #:contract (𝑫 e i)
  #:mode (𝑫 I O)
  [(𝑫𝒓 e () i)
   ----------
   (𝑫 e i)])

(define-judgment-form D  
  #:contract (𝑫𝒓 e r i)
  #:mode (𝑫𝒓 I I O)

  [-----------
   (𝑫𝒓 i r i)]
  
  [(where i (lookup r x))
   -----------
   (𝑫𝒓 x r i)]

  [(𝑫𝒓 e_0 r i_0) (where r_1 (ext r x i_0)) (𝑫𝒓 e_1 r_1 i_1)
   -----
   (𝑫𝒓 (let ((x e_0)) e_1) r i_1)]
   
  [(𝑫𝒓 e_0 r i_0) (where i_1 ,(+ (term i_0) 1))
   -----------
   (𝑫𝒓 (add1 e_0) r i_1)]
  
  [(𝑫𝒓 e_0 r i_0) (where i_1 ,(- (term i_0) 1))
   -----------
   (𝑫𝒓 (sub1 e_0) r i_1)])

(define-metafunction D
  ext : r x i -> r
  [(ext ((x_0 i_0) ...) x i)
   ((x i) (x_0 i_0) ...)])

(define-metafunction D
  lookup : r x -> i or undefined
  [(lookup () x) undefined]
  [(lookup ((x i) (x_1 i_1) ...) x) i]
  [(lookup ((x_0 i_0) (x_1 i_1) ...) x)
   (lookup ((x_1 i_1) ...) x)])

(module+ test
  (test-judgment-holds (𝑫 7 7))
  (test-judgment-holds (𝑫 (add1 7) 8))
  (test-judgment-holds (𝑫 (let ((x 7)) 8) 8))
  (test-judgment-holds (𝑫 (let ((x 7)) x) 7))
  (test-judgment-holds (𝑫 (let ((x 7)) (add1 x)) 8))
  (test-judgment-holds (𝑫 (sub1 (let ((x 7)) (add1 x))) 7))
  (test-judgment-holds (𝑫 (sub1 (let ((x 7))
                                  (let ((y x))
                                    (add1 x))))
                          7))
  (test-judgment-holds (𝑫 (sub1 (let ((x 7))
                                  (let ((x 8))
                                    (add1 x))))
                          8)))

