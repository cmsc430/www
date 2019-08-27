#lang racket
(provide F F-pre 𝑭 𝑭-𝒆𝒏𝒗 lookup ext)
(require redex/reduction-semantics
         (only-in "../extort/semantics.rkt" E 𝑬))

; for use in presentations (informally noting x can't be let, etc.)
(define-extended-language F-pre E
  (e ::= .... x (let ((x e)) e) (p e))
  (p ::= add1 sub1 zero?)
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
  [(𝑭-𝒆𝒏𝒗 e () a)
   ---------- "mt-env"
   (𝑭 e a)])

(define-judgment-form F
  #:contract (𝑭-𝒆𝒏𝒗 e r a)
  #:mode (𝑭-𝒆𝒏𝒗 I I O)

  ;; Value
  [----------- "value"
   (𝑭-𝒆𝒏𝒗 v r v)]

  ;; If
  [(𝑭-𝒆𝒏𝒗 e_0 r v_0) (side-condition (is-true v_0)) (𝑭-𝒆𝒏𝒗 e_1 r a)
   -------- "if-true"
   (𝑭-𝒆𝒏𝒗 (if e_0 e_1 e_2) r a)]

  [(𝑭-𝒆𝒏𝒗 e_0 r v_0) (side-condition (is-false v_0)) (𝑭-𝒆𝒏𝒗 e_2 r a)
   -------- "if-false"
   (𝑭-𝒆𝒏𝒗 (if e_0 e_1 e_2) r a)]

  [(𝑭-𝒆𝒏𝒗 e_0 r err)
   -------- "if-err"
   (𝑭-𝒆𝒏𝒗 (if e_0 e_1 e_2) r err)]

  ;; Let and variable
  [(where a (lookup r x))
   ----------- "var"
   (𝑭-𝒆𝒏𝒗 x r a)]

  [(𝑭-𝒆𝒏𝒗 e_0 r v_0) (𝑭-𝒆𝒏𝒗 e_1 (ext r x v_0) a)
   ----- "let"
   (𝑭-𝒆𝒏𝒗 (let ((x e_0)) e_1) r a)]

  [(𝑭-𝒆𝒏𝒗 e_0 r err)
   ----------- "let-err"
   (𝑭-𝒆𝒏𝒗 (let ((x e_0)) e_1) r err)]

  ;; Primitive application
  [(𝑭-𝒆𝒏𝒗 e_0 r a_0)
   ----------- "prim"
   (𝑭-𝒆𝒏𝒗 (p e_0) r (𝑭-𝒑𝒓𝒊𝒎 (p a_0)))])

(define-metafunction F
  𝑭-𝒑𝒓𝒊𝒎 : (p a) -> a
  [(𝑭-𝒑𝒓𝒊𝒎 (p err)) err]
  [(𝑭-𝒑𝒓𝒊𝒎 (add1 i_0)) ,(+ (term i_0) 1)]
  [(𝑭-𝒑𝒓𝒊𝒎 (sub1 i_0)) ,(- (term i_0) 1)]
  [(𝑭-𝒑𝒓𝒊𝒎 (zero? 0)) #t]
  [(𝑭-𝒑𝒓𝒊𝒎 (zero? i)) #f]
  [(𝑭-𝒑𝒓𝒊𝒎 _) err])

(define-metafunction F
  ext : r x i -> r
  [(ext ((x_0 i_0) ...) x i)
   ((x i) (x_0 i_0) ...)])

(define-metafunction F
  lookup : r x -> a
  [(lookup () x) err]
  [(lookup ((x v) (x_1 v_1) ...) x) v]
  [(lookup ((x_0 v_0) (x_1 v_1) ...) x)
   (lookup ((x_1 v_1) ...) x)])

(define-metafunction F
  is-true : v -> boolean
  [(is-true #f) #f]
  [(is-true v)  #t])

(define-metafunction F
  is-false : v -> boolean
  [(is-false #f) #t]
  [(is-false v)  #f])

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

(module+ test
  (require rackunit)
  ;; Check that the semantics is total function
  (redex-check F e
               (check-true (redex-match? F (a_0) (judgment-holds (𝑭 e a) a)) (term e))
               #:print? #f))
