#lang racket
(provide C C-pre 𝑪 𝑪𝒓 lookup ext)
(require redex/reduction-semantics)

; for use in presentations (informally noting x can't be let, etc.)
(define-language C-pre
  (e ::= i x (add1 e) (sub1 e) (let ((x e)) e))
  (x ::= variable)
  (i ::= integer))

;; the real grammar language
(define-extended-language C C-pre
  (x ::= variable-not-otherwise-mentioned)
  (r ::= ((x i) ...)))

(module+ test
  (test-equal (redex-match? C e (term x)) #t)
  (test-equal (redex-match? C e (term let)) #f)
  (test-equal (redex-match? C e (term (let ((x 1)) x))) #t)
  (test-equal (redex-match? C e (term (let ((let 1)) 3))) #f))

(module+ test
  (test-equal (redex-match? C-pre e (term x)) #t)
  (test-equal (redex-match? C-pre e (term let)) #t)
  (test-equal (redex-match? C-pre e (term (let ((x 1)) x))) #t)
  (test-equal (redex-match? C-pre e (term (let ((let 1)) 3))) #t))

(define-judgment-form C
  #:contract (𝑪 e i)
  #:mode (𝑪 I O)
  [(𝑪𝒓 e () i)
   ----------
   (𝑪 e i)])

(define-judgment-form C  
  #:contract (𝑪𝒓 e r i)
  #:mode (𝑪𝒓 I I O)

  [-----------
   (𝑪𝒓 i r i)]
  
  [(where i (lookup r x))
   -----------
   (𝑪𝒓 x r i)]

  [(𝑪𝒓 e_0 r i_0) (where r_1 (ext r x i_0)) (𝑪𝒓 e_1 r_1 i_1)
   -----
   (𝑪𝒓 (let ((x e_0)) e_1) r i_1)]
   
  [(𝑪𝒓 e_0 r i_0) (where i_1 ,(+ (term i_0) 1))
   -----------
   (𝑪𝒓 (add1 e_0) r i_1)]
  
  [(𝑪𝒓 e_0 r i_0) (where i_1 ,(- (term i_0) 1))
   -----------
   (𝑪𝒓 (sub1 e_0) r i_1)])

(define-metafunction C
  ext : r x i -> r
  [(ext ((x_0 i_0) ...) x i)
   ((x i) (x_0 i_0) ...)])

(define-metafunction C
  lookup : r x -> i or undefined
  [(lookup () x) undefined]
  [(lookup ((x i) (x_1 i_1) ...) x) i]
  [(lookup ((x_0 i_0) (x_1 i_1) ...) x)
   (lookup ((x_1 i_1) ...) x)])

(module+ test
  (test-judgment-holds (𝑪 7 7))
  (test-judgment-holds (𝑪 (add1 7) 8))
  (test-judgment-holds (𝑪 (let ((x 7)) 8) 8))
  (test-judgment-holds (𝑪 (let ((x 7)) x) 7))
  (test-judgment-holds (𝑪 (let ((x 7)) (add1 x)) 8))
  (test-judgment-holds (𝑪 (sub1 (let ((x 7)) (add1 x))) 7))
  (test-judgment-holds (𝑪 (sub1 (let ((x 7))
                                  (let ((y x))
                                    (add1 x))))
                          7))
  (test-judgment-holds (𝑪 (sub1 (let ((x 7))
                                  (let ((x 8))
                                    (add1 x))))
                          8)))

