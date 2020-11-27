#lang racket
(provide C 𝑪)
(require redex/reduction-semantics
         (rename-in (only-in "../con/semantics.rkt" C 𝑪) [C C-] [𝑪 𝑪-]))

(define-extended-language C C-
  (e ::= .... (cond [(zero? e_p0) e_a0] ... [else e_an])))

(define-extended-judgment-form C 𝑪-
  #:mode (𝑪 I O)
  #:contract (𝑪 e i)

  [(𝑪 e_p0 i_p0) ... (!= i_p0 0) ... (𝑪 e_pk 0) (𝑪 e_ak i_ak)
   --------
   (𝑪 (cond [(zero? e_p0) e_a0] ... [(zero? e_pk) e_ak] [(zero? e_pk+1) e_ak+1] ... [else e_an]) i_ak)]

  [(𝑪 e_p0 i_p0) ... (!= i_p0 0) ... (𝑪 e_an i_an)
   --------
   (𝑪 (cond [(zero? e_p0) e_a0] ... [else e_an]) i_an)])


(define-judgment-form C
  #:mode (!= I I)
  #:contract (!= i i)
  [(side-condition (not (= (term i_1) (term i_2))))
   ----
   (!= i_1 i_2)])

