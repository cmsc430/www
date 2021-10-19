#lang racket
(provide H Hm H-concrete 𝑯 𝑯′ 𝑯-𝒆𝒏𝒗 𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 𝑯-𝒑𝒓𝒊𝒎 𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 lookup ext convert unload)
(require redex/reduction-semantics
         (only-in "../fraud/semantics.rkt" G G-concrete))

(define-extended-language H-concrete G-concrete
  (p2 ::= .... cons)
  (p1 ::= .... box unbox car cdr))

(define-extended-language H G
  (p2 ::= .... 'cons)
  (p1 ::= .... 'box 'unbox 'car 'cdr)
  (e  ::= .... (Empty))
  (v ::= .... (box v) (cons v v) '()))


(module+ test
  (test-equal (redex-match? H e (term (Empty))) #t)
  (test-equal (redex-match? H e (term (Prim2 'cons (Int 3) (Empty)))) #t)
  (test-equal (redex-match? H e (term (Prim2 'cons (Var x) (Var y)))) #t)
  (test-equal (redex-match? H v (term (cons 1 2))) #t)
  (test-equal (redex-match? H v (term (cons 1 (cons 2 '())))) #t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (𝑯-𝒆𝒏𝒗 (Prim1 p e_0) r (𝑯-𝒑𝒓𝒊𝒎 p a_0))]

  [(𝑯-𝒆𝒏𝒗 e_0 r a_0)
   (𝑯-𝒆𝒏𝒗 e_1 r a_1)
   ----------- "prim2"
   (𝑯-𝒆𝒏𝒗 (Prim2 p e_0 e_1) r (𝑯-𝒑𝒓𝒊𝒎 p a_0 a_1))])

(define-metafunction H
  𝑯-𝒑𝒓𝒊𝒎 : p a ... -> a
  [(𝑯-𝒑𝒓𝒊𝒎 p v ... err _ ...) err]
  [(𝑯-𝒑𝒓𝒊𝒎 'add1 i_0) ,(+ (term i_0) (term 1))]
  [(𝑯-𝒑𝒓𝒊𝒎 'sub1 i_0) ,(- (term i_0) (term 1))]
  [(𝑯-𝒑𝒓𝒊𝒎 'zero? 0) #t]
  [(𝑯-𝒑𝒓𝒊𝒎 'zero? i) #f]
  [(𝑯-𝒑𝒓𝒊𝒎 '+ i_0 i_1) ,(+ (term i_0) (term i_1))]
  [(𝑯-𝒑𝒓𝒊𝒎 '- i_0 i_1) ,(- (term i_0) (term i_1))]
  [(𝑯-𝒑𝒓𝒊𝒎 'box v) (box v)]
  [(𝑯-𝒑𝒓𝒊𝒎 'unbox (box v)) v]
  [(𝑯-𝒑𝒓𝒊𝒎 'cons v_1 v_2) (cons v_1 v_2)]
  [(𝑯-𝒑𝒓𝒊𝒎 'car (cons v_1 v_2)) v_1]
  [(𝑯-𝒑𝒓𝒊𝒎 'cdr (cons v_1 v_2)) v_2]
  [(𝑯-𝒑𝒓𝒊𝒎 _ ...) err])


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-extended-language Hm_hidden H
  (<intentionally-abstract> ::= (& natural)))

(define-extended-language Hm Hm_hidden
  (α ::= <intentionally-abstract>)
  (v ::= integer boolean (box α) (cons α) '())
  (s ::= (v) (v v))
  (σ ::= ((α s) ...)))

(define-judgment-form Hm
  #:contract (𝑯′ e any)
  #:mode (𝑯′ I O)
  [(𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 e () () σ a)   
   -----------------------
   (𝑯′ e (unload σ a))])


(define-judgment-form Hm
  #:contract (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 e r σ σ a)
  #:mode (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 I I I O O)

  ;; Value
  [----------- "int-lit"
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 (Int i) r σ σ i)]
  [----------- "bool-lit"
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 (Bool b) r σ σ b)]
  [----------- "empty-lit"
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 (Empty) r σ σ '())]

  ;; If
  [(𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 e_0 r σ_0 σ_1 v)
   (side-condition (is-true v))
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 e_0 r σ_1 σ_2 a)
   -------- "if-true"
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 (If e_0 e_1 e_2) r σ_0 σ_2 a)]

  [(𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 e_0 r σ_0 σ_1 v)
   (side-condition (is-false v))
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 e_2 r σ_1 σ_2 a)
   -------- "if-false"
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 (If e_0 e_1 e_2) r σ_0 σ_2 a)]

  [(𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 e_0 r σ_0 σ_1 err)
   -------- "if-err"
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 (If e_0 e_1 e_2) r σ_0 σ_1 err)]

  ;; Let and variable
  [(where a (lookup r x))
   ----------- "var"
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 (Var x) r σ σ a)]

  [(𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 e_0 r σ_0 σ_1 v_0)
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 e_1 (ext r x v_0) σ_1 σ_2 a)
   ----- "let"
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 (Let x e_0 e_1) r σ_0 σ_2 a)]

  [(𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 e_0 r σ_0 σ_1 err)
   ----------- "let-err"
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 (Let x e_0 e_1) r σ_0 σ_1 err)]

  ;; Primitive application
  [(𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 e_0 r σ_0 σ_1 a_0)
   (where (σ_2 a) (𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 p a_0 σ_1))
   ----------- "prim1"
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 (Prim1 p e_0) r σ_0 σ_2 a)]
  
  [(𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 e_0 r σ_0 σ_1 a_0)
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 e_1 r σ_1 σ_2 a_1)
   (where (σ_3 a) (𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 p a_0 a_1 σ_2))
   ----------- "prim2"
   (𝑯-𝒎𝒆𝒎-𝒆𝒏𝒗 (Prim2 p e_0 e_1) r σ_0 σ_3 a)])

(define-metafunction Hm
  𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 : p a ... σ -> (σ a)
  [(𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 p v ... err _ ... σ) (σ err)]
  [(𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 'add1 i_0 σ)          (σ ,(+ (term i_0) 1))]
  [(𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 'sub1 i_0 σ)          (σ ,(- (term i_0) 1))]
  [(𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 'zero? 0 σ)           (σ #t)]
  [(𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 'zero? i σ)           (σ #f)]
  [(𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 '+ i_0 i_1 σ)         (σ ,(+ (term i_0) (term i_1)))]
  [(𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 '- i_0 i_1 σ)         (σ ,(- (term i_0) (term i_1)))]
  [(𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 'box v σ)             (alloc σ (box v))]
  [(𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 'unbox (box α) σ)     (σ v) (where (_ ... (α (v)) _ ...) σ)]
  [(𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 'cons v_1 v_2 σ)      (alloc σ (cons v_1 v_2))]
  [(𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 'car (cons α) σ)      (σ v) (where (_ ... (α (v _)) _ ...) σ)]
  [(𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 'cdr (cons α) σ)      (σ v) (where (_ ... (α (_ v)) _ ...) σ)]
  [(𝑯-𝒎𝒆𝒎-𝒑𝒓𝒊𝒎 _ ... σ) (σ err)])

(define-metafunction Hm
  alloc : σ (_ v ...) -> (σ v)
  [(alloc () (any_cons v ...)) ((((& 0) (v ...))) (any_cons (& 0)))]
  [(alloc ((α_0 s_0) ... ((& i) s_n)) (any_cons v ...))
   (((α_0 s_0) ... ((& i) s_n) ((& ,(add1 (term i))) (v ...)))
    (any_cons (& ,(add1 (term i)))))])


(define-metafunction Hm
  unload : σ a -> any_H_a
  [(unload σ err) err]
  [(unload σ i) i]
  [(unload σ b) b]
  [(unload σ '()) '()]
  [(unload σ (box α))
   (box (unload σ v))
   (where (_ ... (α (v)) _ ...) σ)]
  [(unload σ (cons α))
   (cons (unload σ v_1)
         (unload σ v_2))
   (where (_ ... (α (v_1 v_2)) _ ...) σ)])
                


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convert v to using Racket pairs, boxes, and null
(define-metafunction H
  convert : a -> any
  [(convert '()) ()]
  [(convert (box v_0)) ,(box (term (convert v_0)))]
  [(convert (cons v_0 v_1)) ,(cons (term (convert v_0)) (term (convert v_1)))]
  [(convert a) a])

(module+ test
  (test-judgment-holds (𝑯 (Int 7) 7))
  (test-judgment-holds (𝑯 (Prim1 'add1 (Int 7)) 8))

  (test-judgment-holds (𝑯 (Prim1 'add1 (Bool #f)) err))

  (test-judgment-holds (𝑯 (Let x (Int 7) (Int 8)) 8))
  (test-judgment-holds (𝑯 (Let x (Int 7) (Var x)) 7)) 
  (test-judgment-holds (𝑯 (Let x (Int 7) (Prim1 'add1 (Var x))) 8))
  (test-judgment-holds (𝑯 (Prim1 'sub1 (Let x (Int 7) (Prim1 'add1 (Var x)))) 7))
  (test-judgment-holds (𝑯 (Prim1 'sub1 (Let x (Int 7)
                                            (Let y (Var x)
                                                 (Prim1 'add1 (Var x)))))
                          7))
  (test-judgment-holds (𝑯 (Prim1 'sub1 (Let x (Int 7)
                                            (Let x (Int 8)
                                                 (Prim1 'add1 (Var x)))))
                          8))

  (test-judgment-holds (𝑯 (Prim1 'zero? (Int 0)) #t))
  (test-judgment-holds (𝑯 (Prim1 'zero? (Int 1)) #f))
  (test-judgment-holds (𝑯 (Prim1 'zero? (Bool #f)) err))

  (test-judgment-holds (𝑯 (Prim2 '+ (Int 1) (Int 2)) 3))
  (test-judgment-holds (𝑯 (Prim2 '- (Int 1) (Int 2)) -1))
  (test-judgment-holds (𝑯 (Prim1 'add1 (Bool #f)) err))
  (test-judgment-holds (𝑯 (If (Prim1 'add1 (Bool #f)) (Int 1) (Int 2)) err))
  (test-judgment-holds (𝑯 (If (Prim1 'zero? (Bool #t)) (Prim1 'add1 (Bool #f)) (Int 2)) err)) 
  (test-judgment-holds (𝑯 (Prim2 '+ (Int 1) (Prim1 'add1 (Bool #f))) err))
  (test-judgment-holds (𝑯 (Prim2 '+ (Int 1) (Bool #f)) err))
  (test-judgment-holds (𝑯 (Prim2 '- (Int 1) (Bool #f)) err))
  (test-judgment-holds (𝑯 (Prim2 '- (Prim1 'add1 (Bool #f)) (Bool #f)) err))

  (test-judgment-holds (𝑯 (Empty) '()))
  (test-judgment-holds (𝑯 (Prim2 'cons (Int 1) (Int 2)) (cons 1 2)))
  (test-judgment-holds (𝑯 (Prim2 'cons (Int 1) (Prim1 'add1 (Bool #f))) err))
  (test-judgment-holds (𝑯 (Let x (Int 1)
                            (Let y (Int 2)
                              (Prim2 'cons (Var x) (Var y))))
                          (cons 1 2)))
  (test-judgment-holds (𝑯 (Prim1 'car (Prim2 'cons (Int 1) (Int 2))) 1))
  (test-judgment-holds (𝑯 (Prim1 'cdr (Prim2 'cons (Int 1) (Int 2))) 2))
  (test-judgment-holds (𝑯 (Prim1 'cdr (Prim2 'cons (Int 1) (Prim2 'cons (Int 2) (Empty)))) (cons 2 '())))
  (test-judgment-holds (𝑯 (Prim1 'car (Prim2 'cons (Prim1 'add1 (Int 7)) (Empty))) 8))
  (test-judgment-holds (𝑯 (Prim1 'box (Int 7)) (box 7)))
  (test-judgment-holds (𝑯 (Prim1 'unbox (Prim1 'box (Int 7))) 7))
  (test-judgment-holds (𝑯 (Prim1 'unbox (Prim1 'unbox (Int 7))) err))

  (test-equal (term (convert '())) '())
  (test-equal (term (convert (cons 1 2))) '(1 . 2)))

(module+ test
  (test-judgment-holds (𝑯′ (Int 7) 7)) 
  (test-judgment-holds (𝑯′ (Prim1 'add1 (Int 7)) 8))

  (test-judgment-holds (𝑯′ (Prim1 'add1 (Bool #f)) err))

  (test-judgment-holds (𝑯′ (Let x (Int 7) (Int 8)) 8))
  (test-judgment-holds (𝑯′ (Let x (Int 7) (Var x)) 7)) 
  (test-judgment-holds (𝑯′ (Let x (Int 7) (Prim1 'add1 (Var x))) 8))
  (test-judgment-holds (𝑯′ (Prim1 'sub1 (Let x (Int 7) (Prim1 'add1 (Var x)))) 7))
  (test-judgment-holds (𝑯′ (Prim1 'sub1 (Let x (Int 7)
                                             (Let y (Var x)
                                                  (Prim1 'add1 (Var x)))))
                          7))  
  (test-judgment-holds (𝑯′ (Prim1 'sub1 (Let x (Int 7)
                                             (Let x (Int 8)
                                                  (Prim1 'add1 (Var x)))))
                          8))

  (test-judgment-holds (𝑯′ (Prim1 'zero? (Int 0)) #t))
  (test-judgment-holds (𝑯′ (Prim1 'zero? (Int 1)) #f))
  (test-judgment-holds (𝑯′ (Prim1 'zero? (Bool #f)) err))

  (test-judgment-holds (𝑯′ (Prim2 '+ (Int 1) (Int 2)) 3))
  (test-judgment-holds (𝑯′ (Prim2 '- (Int 1) (Int 2)) -1))
  (test-judgment-holds (𝑯′ (Prim1 'add1 (Bool #f)) err))
  (test-judgment-holds (𝑯′ (If (Prim1 'add1 (Bool #f)) (Int 1) (Int 2)) err))
  (test-judgment-holds (𝑯′ (If (Prim1 'zero? (Bool #t)) (Prim1 'add1 (Bool #f)) (Int 2)) err)) 
  (test-judgment-holds (𝑯′ (Prim2 '+ (Int 1) (Prim1 'add1 (Bool #f))) err))
  (test-judgment-holds (𝑯′ (Prim2 '+ (Int 1) (Bool #f)) err))
  (test-judgment-holds (𝑯′ (Prim2 '- (Int 1) (Bool #f)) err))
  (test-judgment-holds (𝑯′ (Prim2 '- (Prim1 'add1 (Bool #f)) (Bool #f)) err))

  (test-judgment-holds (𝑯′ (Empty) '()))
  (test-judgment-holds (𝑯′ (Prim2 'cons (Int 1) (Int 2)) (cons 1 2)))
  (test-judgment-holds (𝑯′ (Prim2 'cons (Int 1) (Prim1 'add1 (Bool #f))) err))
  (test-judgment-holds (𝑯′ (Let x (Int 1)
                            (Let y (Int 2)
                              (Prim2 'cons (Var x) (Var y))))
                          (cons 1 2)))
  (test-judgment-holds (𝑯′ (Prim1 'car (Prim2 'cons (Int 1) (Int 2))) 1))
  (test-judgment-holds (𝑯′ (Prim1 'cdr (Prim2 'cons (Int 1) (Int 2))) 2))
  (test-judgment-holds (𝑯′ (Prim1 'cdr (Prim2 'cons (Int 1) (Prim2 'cons (Int 2) (Empty)))) (cons 2 '())))
  (test-judgment-holds (𝑯′ (Prim1 'car (Prim2 'cons (Prim1 'add1 (Int 7)) (Empty))) 8))
  (test-judgment-holds (𝑯′ (Prim1 'box (Int 7)) (box 7)))
  (test-judgment-holds (𝑯′ (Prim1 'unbox (Prim1 'box (Int 7))) 7))
  (test-judgment-holds (𝑯′ (Prim1 'unbox (Prim1 'unbox (Int 7))) err)))



(module+ test
  ;; Check that the semantics is total function
  (redex-check H e (redex-match? H (a_0) (judgment-holds (𝑯 e a) a))))
