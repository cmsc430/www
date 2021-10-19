#lang racket
(provide accepts)

;; type Regexp =
;; | 'zero
;; | 'one
;; | `(char ,Char)
;; | `(times ,Regexp ,Regexp)
;; | `(plus ,Regexp ,Regexp)
;; | `(star ,Regexp)

;; type K =
;; | '(k0)
;; | `(k1 ,Regexp ,K)
;; | `(k2 ,K ,Regexp)

;; Regexp String -> Boolean
(define (accepts r s)
  (matcher r (string->list s) '(k0)))

;; Regexp (Listof Char) K -> Bool
(define (matcher r cs k)
  (match r
    ['zero #f]
    ['one (apply-k k cs)]
    [`(char ,c)
     (match cs
       ['() #f]
       [(cons d cs)
        (and (char=? c d) (apply-k k cs))])]
    [`(plus ,r1 ,r2)
     (or (matcher r1 cs k) (matcher r2 cs k))]
    [`(times ,r1 ,r2)
     (matcher r1 cs `(k1 ,r2 ,k))]
    [`(star ,r)
     (apply-k `(k2 ,k ,r) cs)]))

;; K (Listof Char) -> Bool
(define (apply-k k cs)
  (match k
    [`(k0) (empty? cs)]
    [`(k1 ,r2 ,k) (matcher r2 cs k)]
    [`(k2 ,k* ,r) (or (apply-k k* cs) (matcher r cs k))]))
