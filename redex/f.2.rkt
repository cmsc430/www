#lang racket
(require redex/reduction-semantics)

(define-language F.2
  (P ::= (s ... d ... e))
  (Δ ::= (d ...))
  (Σ ::= (s ...))
  (d ::= (define x l))
  (s ::= (c [x : t] ...))
  (e ::= i x o (e e ...) l
     (match e (p e) ...))
  (l ::= 
     (λ ([x : t] ...) e)
     (λ [x : t *] e)
     (λ ([x : t] ... [x : t *]) e))
  (o ::= zero? = + - *)
  (p ::= (c x ...) i x)
  (i ::= integer)
  (c ::= variable)
  (x ::= variable)
  (v ::= i (l ρ) o (c v ...))
  (ρ ::= ((x v) ...))
  (t ::= int bool (list t) (t ... -> t) (t ... t * -> t)))

(define-judgment-form F.2
  #:mode (eval I I I O)
  #:contract (eval Δ ρ e v)
  
  [--------------- int
   (eval Δ ρ i i)]

  [--------------- op
   (eval Δ ρ o o)]

  [(where l (dlookup Δ x))
   ------------------- def
   (eval Δ ρ x (l ρ))]

  [(where v (lookup ρ x))
   --------------- var
   (eval Δ ρ x v)]

  [------------------- clo
   (eval Δ ρ l (l ρ))]
  
  [(eval Δ ρ e_0 zero?)
   (eval Δ ρ e_1 0)
   ---------------------------- zero?-t
   (eval Δ ρ (e_0 e_1) (true))]
  
  [(eval Δ ρ e_0 zero?)
   (eval Δ ρ e_1 n)
   (side-condition ,(not (zero? (term n))))
   ----------------------------- zero?-f
   (eval Δ ρ (e_0 e_1) (false))]
  
  [(eval Δ ρ e_0 =)
   (eval Δ ρ e_1 i_1)
   (eval Δ ρ e_2 i_2)
   ...
   (side-condition ,(apply = (term i_1) (term (i_2 ...))))
   ---------------------------------- =-t
   (eval Δ ρ (e_0 e_1 e_2 ...) (true))]

  [(eval Δ ρ e_0 =)
   (eval Δ ρ e_1 i_1)
   (eval Δ ρ e_2 i_2)
   ...
   (side-condition ,(not (apply = (term i_1) (term (i_2 ...)))))
   ---------------------------------- =-f
   (eval Δ ρ (e_0 e_1 e_2 ...) (true))]

  ;; generalize to n-ary apply
  [(eval Δ ρ e_0 apply)
   (eval Δ ρ e_1 ((λ ([x : t] ...) e) ρ_1))
   (eval Δ ρ e_2 v_l)
   (where (v_1 ...) (from-list v_l))
   (eval Δ (ext ρ_1 ((x v_1) ...)) e v)   
   --------------------------- apply
   (eval Δ ρ (e_0 e_1 e_2) v)]

  ;; ...

  [(eval Δ ρ e ((λ ([x : t] ..._1) e_0) ρ_0))
   (eval Δ ρ e_1 v_1) ...
   (eval Δ (ext ρ_0 ((x v_1) ...)) e_0 v)
   ------------------------- app
   (eval Δ ρ (e e_1 ..._1) v)]

  [(eval Δ ρ e ((λ [x : t *] e_0) ρ_0))
   (eval Δ ρ e_1 v_1) ...
   (eval Δ (ext ρ_0 ((x (to-list v_1 ...)))) e_0 v)
   ------------------------- app-*
   (eval Δ ρ (e e_1 ...) v)]

  [(eval Δ ρ e ((λ ([x_1 : t_1] ..._1 [x_r : t_r] *) e_0) ρ_0))
   (eval Δ ρ e_1 v_1) ...
   (eval Δ ρ e_r v_r) ...
   (eval Δ (ext ρ_0 ((x_1 v_1) ... (x_r (to-list v_r ...)))) e_0 v)
   ----------------------------------- app-rest
   (eval Δ ρ (e e_1 ..._1 e_r ...) v)]
  
  [(eval Δ ρ e_1 v_1) ...
   ;; c not in dom(ρ), dom(Δ)
   ----------------------------------- con
   (eval Δ ρ (c e_1 ...) (c v_1 ...))]

  [(eval Δ ρ e_0 v_0)
   (where (e ρ_′) (vmatch v_0 [p_1 e_1] ...))
   (eval Δ (ext ρ ρ_′) e v)
   --------------------------------------- match
   (eval Δ ρ (match e_0 [p_1 e_1] ...) v)]

  
  )

(define-metafunction F.2
  vmatch : v (p e) ... -> (e ρ) or #f
  [(vmatch v) #f]
  [(vmatch (c v_1 ..._1) [(c x_1 ..._1) e] _ ...)
   (e ((x_1 v_1) ...))]
  [(vmatch v [x e] _ ...) (e ((x v)))]
  [(vmatch v _ [p e] ...) (vmatch v [p e] ...)])

(define-metafunction F.2
  to-list : v ... -> v
  [(to-list) (empty)]
  [(to-list v v_0 ...) (cons v (to-list v_0 ...))])

(define-metafunction F.2
  from-list : v -> (v ...) or #f
  [(from-list (empty)) ()]
  [(from-list (cons v_1 v_r))
   (v_1 v_2 ...)
   (where (v_2 ...) (from-list v_r))])

(define-metafunction F.2
  lookup : ρ x -> v or #f
  [(lookup () x) #f]
  [(lookup ((x v) _ ...) x) v]
  [(lookup (_ (x_0 v_0) ...) x)
   (lookup ((x_0 v_0) ...) x)])

(define-metafunction F.2
  dlookup : Δ x -> l or #f
  [(dlookup () x) #f]
  [(dlookup ((define x l) _ ...) x) l]
  [(dlookup (_ (define x_0 l_0) ...) x)
   (dlookup ((define x_0 l_0) ...) x)])

(define-metafunction F.2
  ext : ρ ρ -> ρ
  [(ext ((x_0 v_0) ...) ((x_1 v_1) ...))
   ((x_1 v_1) ... (x_0 v_0) ...)])
  
  

(judgment-holds (eval {} () (zero? 0) (true)))
(judgment-holds (eval {} () (zero? 8) (false)))
(judgment-holds (eval {} () (= 8) (true)))
(judgment-holds (eval {} () (= 8 8) (true)))
(judgment-holds (eval {} () (= 8 8 8) (true)))


(judgment-holds (eval {} ()
                      (match (true 3)
                        [(false) 2]
                        [(true x) x]
                        [(true x) 4]
                        )
                      3))

(judgment-holds (eval {} ()
                      (λ ([x : int]) x)
                      ((λ ([x : int]) x) ())))

(judgment-holds (eval {} ()
                      (λ ([x : int]) x)
                      ((λ ([x : int]) x) ())))

(judgment-holds (eval {(define f (λ ([x : int]) x))} ()
                      f
                      ((λ ([x : int]) x) ())))

(judgment-holds (eval {(define f (λ ([x : int]) x))} ()
                      (f 1)
                      v)
                v)

(judgment-holds (eval {(define f (λ [x : int] * x))} ()
                      (f 1 2 3)
                      v)
                v)
