#lang racket
(((λ (t)
    ((λ (f) (t (λ (z) ((f f) z))))
     (λ (f) (t (λ (z) ((f f) z))))))
  (λ (tri)
    (λ (n)
      (if (zero? n)
          0
          (+ n (tri (sub1 n)))))))
 36)
