#lang racket
(provide (all-defined-out))

(module+ test
  (require rackunit))

;; type Bt =
;; | `leaf
;; | `(node ,Integer ,Bt ,Bt)

;; Bt -> Boolean
;; Is the binary tree empty?
(define (bt-empty? bt)
  (match bt
    ['leaf #t]
    [(cons 'node _) #f]))

(module+ test
  (check-equal? (bt-empty? 'leaf) #t)
  (check-equal? (bt-empty? '(node 3
                                  (node 7 leaf leaf)
                                  (node 9 leaf leaf)))
                #f))

;; Bt -> Natural
;; Compute the height of a binary tree
(define (bt-height bt)
  (match bt
    [`leaf 0]
    [`(node ,_ ,left ,right)
     (+ 1 (max (bt-height left)
               (bt-height right)))]))

(module+ test
  (check-equal? (bt-height 'leaf) 0)
  (check-equal? (bt-height '(node 3 leaf leaf)) 1)
  (check-equal? (bt-height '(node 2 leaf (node 1 leaf leaf)))
                2))
