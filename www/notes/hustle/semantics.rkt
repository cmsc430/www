#lang racket
(provide H 𝑯 𝑯-𝒆𝒏𝒗 𝑯-𝒑𝒓𝒊𝒎 lookup ext convert)
(require redex/reduction-semantics
         (only-in "../grift/semantics.rkt" G))

(define-extended-language H G
  (p2 ::= .... cons)
  (p1 ::= .... box unbox car cdr)
  (v ::= .... (box v) (cons v v) '()))

(module+ test
  (test-equal (redex-match? H e (term '())) #t)
  (test-equal (redex-match? H e (term (cons 3 '()))) #t)
  (test-equal (redex-match? H e (term (cons x y))) #t)
  (test-equal (redex-match? H v (term (cons 1 2))) #t)
  (test-equal (redex-match? H v (term (cons 1 (cons 2 '())))) #t))


(define-judgment-form H
  #:contract (𝑯 e a)
  #:mode (𝑯 I O)
  [(𝑯-𝒆𝒏𝒗 e () a)
   ----------
   (𝑯 e a)])

;; Identical to 𝑮-𝒆𝒏𝒗
(define-judgment-form H
  #:contract (𝑯-𝒆𝒏𝒗 e r a)
  #:mode (𝑯-𝒆𝒏𝒗 I I O)

  ;; Value
  [-----------
   (𝑯-𝒆𝒏𝒗 v r v)]

  ;; If
  [(𝑯-𝒆𝒏𝒗 e_0 r v_0) (side-condition (is-true v_0)) (𝑯-𝒆𝒏𝒗 e_1 r a)
   --------
   (𝑯-𝒆𝒏𝒗 (if e_0 e_1 e_2) r a)]

  [(𝑯-𝒆𝒏𝒗 e_0 r v_0) (side-condition (is-false v_0)) (𝑯-𝒆𝒏𝒗 e_2 r a)
   --------
   (𝑯-𝒆𝒏𝒗 (if e_0 e_1 e_2) r a)]

  [(𝑯-𝒆𝒏𝒗 e_0 r err)
   --------
   (𝑯-𝒆𝒏𝒗 (if e_0 e_1 e_2) r err)]

  ;; Let and variable
  [(where a (lookup r x))
   -----------
   (𝑯-𝒆𝒏𝒗 x r a)]

  [(𝑯-𝒆𝒏𝒗 e_0 r v_0) (𝑯-𝒆𝒏𝒗 e_1 (ext r x v_0) a)
   -----
   (𝑯-𝒆𝒏𝒗 (let ((x e_0)) e_1) r a)]

  [(𝑯-𝒆𝒏𝒗 e_0 r err)
   -----------
   (𝑯-𝒆𝒏𝒗 (let ((x e_0)) e_1) r err)]

  ;; Primitive application
  [(𝑯-𝒆𝒏𝒗 e_0 r a_0) ...
   -----------
   (𝑯-𝒆𝒏𝒗 (p e_0 ...) r (𝑯-𝒑𝒓𝒊𝒎 (p a_0 ...)))])

(define-metafunction H
  𝑯-𝒑𝒓𝒊𝒎 : (p a ...) -> a
  [(𝑯-𝒑𝒓𝒊𝒎 (p v ... err _ ...)) err]
  [(𝑯-𝒑𝒓𝒊𝒎 (add1 i_0)) ,(+ (term i_0) 1)]
  [(𝑯-𝒑𝒓𝒊𝒎 (sub1 i_0)) ,(- (term i_0) 1)]
  [(𝑯-𝒑𝒓𝒊𝒎 (zero? 0)) #t]
  [(𝑯-𝒑𝒓𝒊𝒎 (zero? i)) #f]
  [(𝑯-𝒑𝒓𝒊𝒎 (+ i_0 i_1)) ,(+ (term i_0) (term i_1))]
  [(𝑯-𝒑𝒓𝒊𝒎 (- i_0 i_1)) ,(- (term i_0) (term i_1))]
  [(𝑯-𝒑𝒓𝒊𝒎 (box v)) (box v)]
  [(𝑯-𝒑𝒓𝒊𝒎 (unbox (box v))) v]
  [(𝑯-𝒑𝒓𝒊𝒎 (cons v_1 v_2)) (cons v_1 v_2)]
  [(𝑯-𝒑𝒓𝒊𝒎 (car (cons v_1 v_2))) v_1]
  [(𝑯-𝒑𝒓𝒊𝒎 (cdr (cons v_1 v_2))) v_2]  
  [(𝑯-𝒑𝒓𝒊𝒎 _) err])

(define-metafunction H
  ext : r x v -> r
  [(ext ((x_0 v_0) ...) x v)
   ((x v) (x_0 v_0) ...)])

(define-metafunction H
  lookup : r x -> a
  [(lookup () x) err]
  [(lookup ((x v) (x_1 v_1) ...) x) v]
  [(lookup ((x_0 v_0) (x_1 v_1) ...) x)
   (lookup ((x_1 v_1) ...) x)])

(define-metafunction H
  is-true : v -> boolean
  [(is-true #f) #f]
  [(is-true v)  #t])

(define-metafunction H
  is-false : v -> boolean
  [(is-false #f) #t]
  [(is-false v)  #f])


;; Convert v to using Racket pairs, boxes, and null
(define-metafunction H
  convert : a -> any
  [(convert '()) ()]
  [(convert (box v_0)) ,(box (term (convert v_0)))]
  [(convert (cons v_0 v_1)) ,(cons (term (convert v_0)) (term (convert v_1)))]
  [(convert a) a])

(module+ test
  (test-judgment-holds (𝑯 7 7))
  (test-judgment-holds (𝑯 (add1 7) 8))

  (test-judgment-holds (𝑯 (add1 #f) err))

  (test-judgment-holds (𝑯 (let ((x 7)) 8) 8))
  (test-judgment-holds (𝑯 (let ((x 7)) x) 7))
  (test-judgment-holds (𝑯 (let ((x 7)) (add1 x)) 8))
  (test-judgment-holds (𝑯 (sub1 (let ((x 7)) (add1 x))) 7))
  (test-judgment-holds (𝑯 (sub1 (let ((x 7))
                                  (let ((y x))
                                    (add1 x))))
                          7))
  (test-judgment-holds (𝑯 (sub1 (let ((x 7))
                                  (let ((x 8))
                                    (add1 x))))
                          8))

  (test-judgment-holds (𝑯 (zero? 0) #t))
  (test-judgment-holds (𝑯 (zero? 1) #f))
  (test-judgment-holds (𝑯 (zero? #f) err))

  (test-judgment-holds (𝑯 (+ 1 2) 3))
  (test-judgment-holds (𝑯 (- 1 2) -1))
  (test-judgment-holds (𝑯 (add1 #f) err))
  (test-judgment-holds (𝑯 (if (add1 #f) 1 2) err))
  (test-judgment-holds (𝑯 (if (zero? #t) (add1 #f) 2) err))
  (test-judgment-holds (𝑯 (+ 1 (add1 #f)) err))
  (test-judgment-holds (𝑯 (+ 1 #f) err))
  (test-judgment-holds (𝑯 (- 1 #f) err))
  (test-judgment-holds (𝑯 (- (add1 #f) #f) err))

  (test-judgment-holds (𝑯 '() '()))
  (test-judgment-holds (𝑯 (cons 1 2) (cons 1 2)))
  (test-judgment-holds (𝑯 (cons 1 (add1 #f)) err))
  (test-judgment-holds (𝑯 (let ((x 1))
                            (let ((y 2))
                              (cons x y)))
                          (cons 1 2)))

  (test-judgment-holds (𝑯 (car (cons 1 2)) 1))
  (test-judgment-holds (𝑯 (cdr (cons 1 2)) 2))
  (test-judgment-holds (𝑯 (cdr (cons 1 (cons 2 '()))) (cons 2 '())))
  (test-judgment-holds (𝑯 (car (cons (add1 7) '())) 8))

  (test-judgment-holds (𝑯 (box 7) (box 7)))
  (test-judgment-holds (𝑯 (unbox (box 7)) 7))
  (test-judgment-holds (𝑯 (unbox 7) 'err))

  (test-equal (term (convert '())) '())
  (test-equal (term (convert (cons 1 2))) '(1 . 2)))



(module+ test
  ;; Check that the semantics is total function
  (redex-check H e (redex-match? H (a_0) (judgment-holds (𝑯 e a) a))))
