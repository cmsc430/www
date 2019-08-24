#lang racket
(provide F F-pre 𝑭 𝑭𝒓 lookup ext)
(require redex/reduction-semantics
         (only-in "../extort/semantics.rkt" E 𝑬))

; for use in presentations (informally noting x can't be let, etc.)
(define-extended-language F-pre E
  (e ::= .... x (let ((x e)) e))
  (x ::= variable))

;; the real grammar language
(define-extended-language F F-pre
  (x ::= variable-not-otherwise-mentioned)
  (r ::= ((x i) ...)))

(module+ test
  (test-equal (redex-match? F e (term x)) #t)
  (test-equal (redex-match? F e (term let)) #f)
  (test-equal (redex-match? F e (term (let ((x 1)) x))) #t)
  (test-equal (redex-match? F e (term (let ((let 1)) 3))) #f))

(module+ test
  (test-equal (redex-match? F-pre e (term x)) #t)
  (test-equal (redex-match? F-pre e (term let)) #t)
  (test-equal (redex-match? F-pre e (term (let ((x 1)) x))) #t)
  (test-equal (redex-match? F-pre e (term (let ((let 1)) 3))) #t))

(define-judgment-form F
  #:contract (𝑭 e a)
  #:mode (𝑭 I O)
  [(𝑭𝒓 e () a)
   ----------
   (𝑭 e a)])

(define-judgment-form F  
  #:contract (𝑭𝒓 e r a)
  #:mode (𝑭𝒓 I I O)

  ;; New
  [(where v (lookup r x))
   -----------
   (𝑭𝒓 x r v)]

  [(𝑭𝒓 e_0 r v_0) (where r_1 (ext r x v_0)) (𝑭𝒓 e_1 r_1 v_1)
   -----
   (𝑭𝒓 (let ((x e_0)) e_1) r v_1)]

  ;; Extension of Dupe's semantics
  [-----------
   (𝑭𝒓 v r v)]

  [(𝑭𝒓 e_0 r i_0) (where i_1 ,(+ (term i_0) 1))
   -----------
   (𝑭𝒓 (add1 e_0) r i_1)]
  
  [(𝑭𝒓 e_0 r i_0) (where i_1 ,(- (term i_0) 1))
   -----------
   (𝑭𝒓 (sub1 e_0) r i_1)]

  [(𝑭𝒓 e_0 r i) (side-condition ,(= (term i) 0))
   -----------
   (𝑭𝒓 (zero? e_0) r #t)]

  [(𝑭𝒓 e_0 r i) (side-condition ,(!= (term i) 0))
   -----------
   (𝑭𝒓 (zero? e_0) r #f)]

  [(𝑭𝒓 e_0 r v_0) (is-true v_0) (𝑭𝒓 e_1 r v_1)
   --------
   (𝑭𝒓 (if e_0 e_1 e_2) r v_1)]
  
  [(𝑭𝒓 e_0 r v_0) (is-false v_0) (𝑭𝒓 e_2 r v_2)
   --------
   (𝑭𝒓 (if e_0 e_1 e_2) r v_2)]

  ;; Extension of Extort's semantics
  [--------
   (𝑭𝒓 (add1 b) r err)]

  [-----------
   (𝑭𝒓 (sub1 b) r err)]

  [-----------
   (𝑭𝒓 (zero? b) r err)]

  [(𝑭𝒓 e r err)
   -----------
   (𝑭𝒓 (zero? e) r err)]

  [(𝑭𝒓 e r err)
   -----------
   (𝑭𝒓 (add1 e) r err)]

  [(𝑭𝒓 e r err)
   -----------
   (𝑭𝒓 (sub1 e) r err)]

  [(𝑭𝒓 e r err)
   -----------
   (𝑭𝒓 (if e e_0 e_1) r err)]

  ;; Error propagation for Let
  [(𝑭𝒓 e_0 r err)
   -----------
   (𝑭𝒓 (let ((x e_0)) e_1) r err)]

  [(𝑭𝒓 e_0 r v_0) (𝑭𝒓 e_1 r err)
   -----------
   (𝑭𝒓 (let ((x e_0)) e_1) r err)])


(define-judgment-form F
  #:mode (is-true I)
  #:contract (is-true v)
  [-----------
   (is-true #t)]
  [----------
   (is-true i)])

(define-judgment-form F
  #:mode (is-false I)
  #:contract (is-false v)
  [-----------
   (is-false #f)])

(define (!= n m)
  (not (= n m)))

(define-metafunction F
  ext : r x i -> r
  [(ext ((x_0 i_0) ...) x i)
   ((x i) (x_0 i_0) ...)])

(define-metafunction F
  lookup : r x -> i or undefined
  [(lookup () x) undefined]
  [(lookup ((x i) (x_1 i_1) ...) x) i]
  [(lookup ((x_0 i_0) (x_1 i_1) ...) x)
   (lookup ((x_1 i_1) ...) x)])

(module+ test
  (test-judgment-holds (𝑭 7 7))
  (test-judgment-holds (𝑭 (add1 7) 8))

  (test-judgment-holds (𝑭 (add1 #f) err))
  
  (test-judgment-holds (𝑭 (let ((x 7)) 8) 8))
  (test-judgment-holds (𝑭 (let ((x 7)) x) 7))
  (test-judgment-holds (𝑭 (let ((x 7)) (add1 x)) 8))
  (test-judgment-holds (𝑭 (sub1 (let ((x 7)) (add1 x))) 7))
  (test-judgment-holds (𝑭 (sub1 (let ((x 7))
                                  (let ((y x))
                                    (add1 x))))
                          7))
  (test-judgment-holds (𝑭 (sub1 (let ((x 7))
                                  (let ((x 8))
                                    (add1 x))))
                          8)))

