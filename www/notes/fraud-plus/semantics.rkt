#lang racket
(provide F 𝑭-𝒆𝒏𝒗)

(require redex/reduction-semantics
         (rename-in (only-in "../fraud/semantics.rkt" F 𝑭-𝒆𝒏𝒗) [F F-] [𝑭-𝒆𝒏𝒗 𝑭--𝒆𝒏𝒗]))

; for use in presentations (informally noting x can't be let, etc.)
(define-extended-language F F-
  (e ::= .... (cond [e_p0 e_a0] ... [else e_an])))


(define-extended-judgment-form F 𝑭--𝒆𝒏𝒗
  #:contract (𝑭-𝒆𝒏𝒗 e r a)
  #:mode (𝑭-𝒆𝒏𝒗 I I O)
    
  [(𝑭-𝒆𝒏𝒗 e_p0 r v_p0) ... (is-false v_p0) ... (𝑭-𝒆𝒏𝒗 e_pk r v) (is-true v) (𝑭-𝒆𝒏𝒗 e_ak r a)
   --------
   (𝑭-𝒆𝒏𝒗 (cond [e_p0 e_a0] ... [e_pk e_ak] [e_pk+1 e_ak+1] ... [else e_an]) r a)]

  [(𝑭-𝒆𝒏𝒗 e_p0 r v_p0) ... (is-false v_p0) ...  (𝑭-𝒆𝒏𝒗 e_an r a)
   --------
   (𝑭-𝒆𝒏𝒗 (cond [e_p0 e_a0] ... [else e_an]) r a)]
  
  [(𝑭-𝒆𝒏𝒗 e_p0 r v_p0) ... (is-false v_p0) ... (𝑭-𝒆𝒏𝒗 e_pk r err)
   --------
   (𝑭-𝒆𝒏𝒗 (cond [e_p0 e_a0] ... [e_pk e_ak] [e_pk+1 e_ak+1] ... [else e_an]) r err)]
  
#|

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
  |#
  )

(define-judgment-form F
  #:mode (is-true I)
  #:contract (is-true v)
  [----
   (is-true #t)]
  [-----
   (is-true i)])

(define-judgment-form F
  #:mode (is-false I)
  #:contract (is-false v)
  [----
   (is-false #f)])
