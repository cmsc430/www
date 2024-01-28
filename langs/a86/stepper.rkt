#lang racket
(provide main)

(require redex)

(define-language L)

;; A reduction relation that just relates elements
;; of the list to their successors
(define (r ls)
  (define i 0)
  (reduction-relation L
                      (--> any_i
                           any_j
                           (where any_j
                                  ,(begin
                                     (set! i (add1 i))
                                     (list-ref ls (min i (sub1 (length ls)))))))))


;; reads log file from stdin
(define (main)
  (define ls
    (let loop ()
      (if (eof-object? (read))
          '()
          (cons (read) (loop)))))
  
  ;; replace instr indices with their instructions
  (define ls1
    (map (λ (s)
           (map (λ (p)
                  (match p
                    [(list 'instr i)
                     (list 'instr (list-ref (list-ref ls 0)
                                            (add1 i)))]
                    [_ p]))
                s))
         ls))

  ;; run the stepper
  (stepper (r (rest ls1)) (first (rest ls1))))
