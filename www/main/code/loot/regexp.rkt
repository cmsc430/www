#lang racket
(provide accepts)

;; type Regexp =
;; | 'zero
;; | 'one
;; | `(char ,Char)
;; | `(times ,Regexp ,Regexp)
;; | `(plus ,Regexp ,Regexp)
;; | `(star ,Regexp)

;; Regexp String -> Boolean
(define (accepts r s)
  (matcher r (string->list s) (λ (cs) (empty? cs))))

;; Regexp (Listof Char) ((Listof Char) -> Bool) -> Bool
(define (matcher r cs k)
  (match r
    ['zero #f]
    ['one (k cs)]
    [`(char ,c)
     (match cs
       ['() #f]
       [(cons d cs) (and (char=? c d) (k cs))])]
    [`(plus ,r1 ,r2)
     (or (matcher r1 cs k) (matcher r2 cs k))]
    [`(times ,r1 ,r2)
     (matcher r1 cs (λ (cs) (matcher r2 cs k)))]
    [`(star ,r)
     (letrec ((matcher* (λ (cs) (or (k cs) (matcher r cs matcher*)))))
       (matcher* cs))]))
