#lang racket
(provide E 𝑬)
(require redex/reduction-semantics
         (only-in "../dupe/semantics.rkt" D 𝑫))

(define-extended-language E D
  (e ::= ....)
  (a ::= v err))

(define-extended-judgment-form E 𝑫
  #:mode (𝑬 I O)
  #:contract (𝑬 e a)
  [--------
   (𝑬 (add1 b) err)]

  [-----------
   (𝑬 (sub1 b) err)]

  [-----------
   (𝑬 (zero? b) err)]

  [(𝑬 e err)
   -----------
   (𝑬 (zero? e) err)]

  [(𝑬 e err)
   -----------
   (𝑬 (add1 e) err)]

  [(𝑬 e err)
   -----------
   (𝑬 (sub1 e) err)]

  [(𝑬 e err)
   -----------
   (𝑬 (if e e_0 e_1) err)])


(module+ test
  (test-judgment-holds (𝑬 7 7))
  (test-judgment-holds (𝑬 #f #f))
  (test-judgment-holds (𝑬 #t #t))
  (test-judgment-holds (𝑬 (add1 8) 9))
  (test-judgment-holds (𝑬 (sub1 8) 7))

  (test-judgment-holds (𝑬 (if #f 3 4) 4))
  (test-judgment-holds (𝑬 (if #t 3 4) 3))
  (test-judgment-holds (𝑬 (zero? 0) #t))
  (test-judgment-holds (𝑬 (zero? 1) #f))
  (test-judgment-holds (𝑬 (if (zero? 0) 3 4) 3))
  (test-judgment-holds (𝑬 (if (zero? 1) 3 4) 4))

  
  (test-judgment-holds (𝑬 (add1 #t) err))
  (test-judgment-holds (𝑬 (add1 #f) err))
  (test-judgment-holds (𝑬 (sub1 #t) err))
  (test-judgment-holds (𝑬 (sub1 #f) err))
  (test-judgment-holds (𝑬 (zero? #t) err))
  (test-judgment-holds (𝑬 (zero? #f) err))

  (test-judgment-holds (𝑬 (add1 (if #t #t #t)) err))
  (test-judgment-holds (𝑬 (sub1 (if #t #t #t)) err))
  (test-judgment-holds (𝑬 (zero? (if #t #t #t)) err))

  (test-judgment-holds (𝑬 (add1 (zero? #f)) err))
  (test-judgment-holds (𝑬 (sub1 (zero? #f)) err))
  (test-judgment-holds (𝑬 (zero? (zero? #f)) err))
  (test-judgment-holds (𝑬 (if (zero? #f) 1 2) err)))
