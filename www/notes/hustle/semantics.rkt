#lang racket
(provide H H-concrete 𝑯 𝑯-𝒆𝒏𝒗 𝑯-𝒑𝒓𝒊𝒎 lookup ext convert)
(require redex/reduction-semantics
         (only-in "../grift/semantics.rkt" G G-concrete))

(define-extended-language H-concrete G-concrete
  (p2 ::= .... cons)
  (p1 ::= .... box unbox car cdr))

(define-extended-language H G
  (p2 ::= .... cons)
  (p1 ::= .... box unbox car cdr)
  (e  ::= .... (Empty))
  (v ::= .... (box v) (cons v v) '()))

(module+ test
  (test-equal (redex-match? H e (term (Empty))) #t)
  (test-equal (redex-match? H e (term (Prim2 cons (Int 3) (Empty)))) #t)
  (test-equal (redex-match? H e (term (Prim2 cons (Var x) (Var y)))) #t)
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
  [----------- "int-lit"
   (𝑯-𝒆𝒏𝒗 (Int i) r i)]
  [----------- "bool-lit"
   (𝑯-𝒆𝒏𝒗 (Bool b) r b)]
  [----------- "empty-lit"
   (𝑯-𝒆𝒏𝒗 (Empty) r '())]

  ;; If
  [(𝑯-𝒆𝒏𝒗 e_0 r v_0) (side-condition (is-true v_0)) (𝑯-𝒆𝒏𝒗 e_1 r a)
   -------- "if-true"
   (𝑯-𝒆𝒏𝒗 (If e_0 e_1 e_2) r a)]

  [(𝑯-𝒆𝒏𝒗 e_0 r v_0) (side-condition (is-false v_0)) (𝑯-𝒆𝒏𝒗 e_2 r a)
   -------- "if-false"
   (𝑯-𝒆𝒏𝒗 (If e_0 e_1 e_2) r a)]

  [(𝑯-𝒆𝒏𝒗 e_0 r err)
   -------- "if-err"
   (𝑯-𝒆𝒏𝒗 (If e_0 e_1 e_2) r err)]

  ;; Let and variable
  [(where a (lookup r x))
   ----------- "var"
   (𝑯-𝒆𝒏𝒗 (Var x) r a)]

  [(𝑯-𝒆𝒏𝒗 e_0 r v_0) (𝑯-𝒆𝒏𝒗 e_1 (ext r x v_0) a)
   ----- "let"
   (𝑯-𝒆𝒏𝒗 (Let x e_0 e_1) r a)]

  [(𝑯-𝒆𝒏𝒗 e_0 r err)
   ----------- "let-err"
   (𝑯-𝒆𝒏𝒗 (Let x e_0 e_1) r err)]

  ;; Primitive application
  [(𝑯-𝒆𝒏𝒗 e_0 r a_0)
   ----------- "prim1"
   (𝑯-𝒆𝒏𝒗 (Prim1 p e_0) r (𝑯-𝒑𝒓𝒊𝒎 (p a_0)))]

  [(𝑯-𝒆𝒏𝒗 e_0 r a_0)
   (𝑯-𝒆𝒏𝒗 e_1 r a_1)
   ----------- "prim2"
   (𝑯-𝒆𝒏𝒗 (Prim2 p e_0 e_1) r (𝑯-𝒑𝒓𝒊𝒎 (p a_0 a_1)))])

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
  (test-judgment-holds (𝑯 (Int 7) 7))
  (test-judgment-holds (𝑯 (Prim1 add1 (Int 7)) 8))

  (test-judgment-holds (𝑯 (Prim1 add1 (Bool #f)) err))

  (test-judgment-holds (𝑯 (Let x (Int 7) (Int 8)) 8))
  (test-judgment-holds (𝑯 (Let x (Int 7) (Var x)) 7)) 
  (test-judgment-holds (𝑯 (Let x (Int 7) (Prim1 add1 (Var x))) 8))
  (test-judgment-holds (𝑯 (Prim1 sub1 (Let x (Int 7) (Prim1 add1 (Var x)))) 7))
  (test-judgment-holds (𝑯 (Prim1 sub1 (Let x (Int 7)
                                           (Let y (Var x)
                                                (Prim1 add1 (Var x)))))
                          7))  
  (test-judgment-holds (𝑯 (Prim1 sub1 (Let x (Int 7)
                                           (Let x (Int 8)
                                                (Prim1 add1 (Var x)))))
                          8))

  (test-judgment-holds (𝑯 (Prim1 zero? (Int 0)) #t))
  (test-judgment-holds (𝑯 (Prim1 zero? (Int 1)) #f))
  (test-judgment-holds (𝑯 (Prim1 zero? (Bool #f)) err))

  (test-judgment-holds (𝑯 (Prim2 + (Int 1) (Int 2)) 3))
  (test-judgment-holds (𝑯 (Prim2 - (Int 1) (Int 2)) -1))
  (test-judgment-holds (𝑯 (Prim1 add1 (Bool #f)) err))
  (test-judgment-holds (𝑯 (If (Prim1 add1 (Bool #f)) (Int 1) (Int 2)) err))
  (test-judgment-holds (𝑯 (If (Prim1 zero? (Bool #t)) (Prim1 add1 (Bool #f)) (Int 2)) err)) 
  (test-judgment-holds (𝑯 (Prim2 + (Int 1) (Prim1 add1 (Bool #f))) err))
  (test-judgment-holds (𝑯 (Prim2 + (Int 1) (Bool #f)) err))
  (test-judgment-holds (𝑯 (Prim2 - (Int 1) (Bool #f)) err))
  (test-judgment-holds (𝑯 (Prim2 - (Prim1 add1 (Bool #f)) (Bool #f)) err))

  (test-judgment-holds (𝑯 (Empty) '()))
  (test-judgment-holds (𝑯 (Prim2 cons (Int 1) (Int 2)) (cons 1 2)))
  (test-judgment-holds (𝑯 (Prim2 cons (Int 1) (Prim1 add1 (Bool #f))) err))
  (test-judgment-holds (𝑯 (Let x (Int 1)
                            (Let y (Int 2)
                              (Prim2 cons (Var x) (Var y))))
                          (cons 1 2)))
  (test-judgment-holds (𝑯 (Prim1 car (Prim2 cons (Int 1) (Int 2))) 1))
  (test-judgment-holds (𝑯 (Prim1 cdr (Prim2 cons (Int 1) (Int 2))) 2))
  (test-judgment-holds (𝑯 (Prim1 cdr (Prim2 cons (Int 1) (Prim2 cons (Int 2) (Empty)))) (cons 2 '())))
  (test-judgment-holds (𝑯 (Prim1 car (Prim2 cons (Prim1 add1 (Int 7)) (Empty))) 8))
  (test-judgment-holds (𝑯 (Prim1 box (Int 7)) (box 7)))
  (test-judgment-holds (𝑯 (Prim1 unbox (Prim1 box (Int 7))) 7))
  (test-judgment-holds (𝑯 (Prim1 unbox (Prim1 unbox (Int 7))) err))

  (test-equal (term (convert '())) '())
  (test-equal (term (convert (cons 1 2))) '(1 . 2)))



(module+ test
  ;; Check that the semantics is total function
  (redex-check H e (redex-match? H (a_0) (judgment-holds (𝑯 e a) a))))
