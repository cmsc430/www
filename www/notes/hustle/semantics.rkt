#lang racket
(provide H 𝑯)
(require redex/reduction-semantics
         (only-in "../grift/semantics.rkt" G 𝑮 𝑮-r 𝑮-prim 𝑮-type-error)
         (only-in "../fraud/semantics.rkt" F 𝑭 𝑭𝒓))

;; This is so tedious it makes me want to postpone errors until much later....

(define-extended-language H G
  (p2 ::= .... cons)
  (p1 ::= .... car cdr)
  (v ::= .... (cons v v) '()))

(module+ test
  (test-equal (redex-match? H e (term '())) #t)
  (test-equal (redex-match? H e (term (cons 3 '()))) #t)
  (test-equal (redex-match? H e (term (cons x y))) #t)
  (test-equal (redex-match? H v (term (cons 1 2))) #t)
  (test-equal (redex-match? H v (term (cons 1 (cons 2 '())))) #t))


(define-judgment-form H
  #:contract (𝑯 e a)
  #:mode (𝑯 I O)
  [(𝑯-r e () a)
   ----------
   (𝑯 e a)])

;; Can't replace this rule in Gr due to
;; https://github.com/racket/redex/issues/192

(define-extended-judgment-form H 𝑭𝒓
  #:contract (𝑯-r e r a)
  #:mode (𝑯-r I I O)
  [(𝑯-r e_0 r a_0) ... (𝑯-prim (p a_0 ...) a_1)
   ----------- prim
   (𝑯-r (p e_0 ...) r a_1)])
  
(define-extended-judgment-form H 𝑮-prim
  #:contract (𝑯-prim (p a ...) a)
  #:mode     (𝑯-prim I         O)
  [------- cons
   (𝑯-prim (cons v_1 v_2) (cons v_1 v_2))]

  [------- car
   (𝑯-prim (car (cons v_1 v_2)) v_1)]

  [------- cdr
   (𝑯-prim (cdr (cons v_1 v_2)) v_2)]
  
  [(𝑯-type-error (p v ...))
   --------------- type-error
   (𝑯-prim (p v ...) err)])

(define-extended-judgment-form H 𝑮-type-error
  #:contract (𝑯-type-error (p v ...))
  #:mode (𝑯-type-error I)
  [(𝑯-type-error (car b))]
  [(𝑯-type-error (car i))]
  [(𝑯-type-error (car '()))]
  
  [(𝑯-type-error (cdr b))]
  [(𝑯-type-error (cdr i))]
  [(𝑯-type-error (cdr '()))]

  [(𝑯-type-error (add1 '()))]
  [(𝑯-type-error (add1 (cons _ _)))]  
  [(𝑯-type-error (sub1 '()))]
  [(𝑯-type-error (sub1 (cons _ _)))]
  [(𝑯-type-error (+ _ ... '() _ ...))]
  [(𝑯-type-error (+ _ ... (cons _ _) _ ...))]
  [(𝑯-type-error (- _ ... '() _ ...))]
  [(𝑯-type-error (- _ ... (cons _ _) _ ...))])


#|
;; A sketch of how to add boxes and eq? with an
;; explicit heap

(define-extended-language H G
  (p2 ::= .... eq?)
  (p1 ::= .... box unbox)
  (v ::= .... (ptr l))
  (s ::= ((l v) ...))
  (l ::= integer))

(define-judgment-form H
  #:contract (𝑯 e a)
  #:mode (𝑯 I O)
  [(𝑯𝒓𝒔 e () () a)
   ----------
   (𝑯 e a)])

(define-judgment-form H 𝑯𝒓𝒔
  #:contract (𝑯𝒓𝒔 e r s a s)
  #:mode     (𝑯𝒓𝒔 I I I O O)

  [(𝑯𝒓𝒔 e r s_0 v s_1) (where (l s_2) (alloc s_1 v))
   --------
   (𝑯𝒓𝒔 (box e) r s_0 (ptr l) s_2)]

  [(𝑯𝒓𝒔* (e ...) r s_0 (v ...) s_1) (𝑷𝒓𝒊𝒎 (p v ...) a)
   ---
   (𝑯𝒓𝒔 (p e ..) r s_0 a s_1)]

  )

(define-judgment-form H 𝑯𝒓𝒔*
  #:contract (𝑯𝒓𝒔* (e ...) r s a s)
  #:mode     (𝑯𝒓𝒔* I       I I O O)
  [---
   (𝑯𝒓𝒔* () r s () s)]

  [(𝑯𝒓𝒔 e r s_1 a s_2) (𝑯𝒓𝒔* (e_0 ...) r s_1 (a_0 ...) s_3)
   ---
   (𝑯𝒓𝒔* (e e_0 ...) r s_1 (a a_0 ...) s_3)])
|#

  
   
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

  (test-judgment-holds (𝑯 (+ 1 2) 3))
  (test-judgment-holds (𝑯 (- 1 2) -1))
  (test-judgment-holds (𝑯 (add1 #f) err))
  (test-judgment-holds (𝑯 (if (add1 #f) 1 2) err))
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
  )
