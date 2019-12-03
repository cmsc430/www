#lang racket

(begin
  ;; sieve for computing primes, based on GT Benchmarks
  
  ;;--------------------------------------------------------------------------------------------------
  ;; streams
  
  (define (stream hd thunk)
    (cons hd thunk))

  (define (stream-first s)
    (car s))

  (define (stream-rest s)
    (cdr s))

  ;(: make-stream (-> Natural (-> stream) stream))
  (define (make-stream hd thunk)
    (stream hd thunk))

  ;; Destruct a stream into its first value and the new stream produced by de-thunking the tail
  ;(: stream-unfold (-> stream (cons Natural stream)))
  (define (stream-unfold st)
    (cons (stream-first st) ((stream-rest st))))

  ;; [stream-get st i] Get the [i]-th element from the stream [st]
  ;(: stream-get (-> stream Natural Natural))
  (define (stream-get st i)
    (match (stream-unfold st)
      [(cons hd tl) 
       (cond [(= i 0) hd]
             [else    (stream-get tl (sub1 i))])]))

  ;; [stream-take st n] Collect the first [n] elements of the stream [st].
  ;(: stream-take (-> stream Natural (Listof Natural)))
  (define (stream-take st n)
    (cond [(= n 0) '()]
          [else (match (stream-unfold st)
                  [(cons hd tl)
                   (cons hd (stream-take tl (sub1 n)))])]))

  ;;--------------------------------------------------------------------------------------------------
  ;; sieve
  
  ;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
  ;(: count-from (-> Natural stream))
  (define (count-from n)
    (make-stream n (λ () (count-from (add1 n)))))

  ;; `sift n st` Filter all elements in `st` that are equal to `n`.
  ;; Return a new stream.
  ;(: sift (-> Natural stream stream))
  (define (sift n st)
    (match (stream-unfold st)
      [(cons hd tl)  
       (cond [(= 0 (modulo hd n)) (sift n tl)]
             [else (make-stream hd (λ () (sift n tl)))])]))

  ;; `sieve st` Sieve of Eratosthenes
  ;(: sieve (-> stream stream))
  (define (sieve st)
    (match (stream-unfold st)
      [(cons hd tl) 
       (make-stream hd (λ () (sieve (sift hd tl))))]))

  ;; stream of prime numbers
  ;(: primes (-> stream)
  (define (primes) (sieve (count-from 2)))

  ;;--------------------------------------------------------------------------------------------------
  ;; reading numbers
  
  (define (* n m)
    (match n
      [0 0]
      [1 m]
      [n (+ m (* (sub1 n) m))]))

  (define (expt n m)
    (match m
      [0 1]
      [1 n]
      [m (* n (expt n (sub1 m)))]))
  
  (define (read-number)
    (digits->number (read-digits)))

  (define (read-digits)
    (let ((c (read-char)))
      (if (eof-object? c)
          '()
          (if (eq? #\newline c)
              '()
              (cons c (read-digits))))))

  (define (digits->number ds)
    (match ds
      ['() 0]
      [(cons d ds)
       (+ (* (digit->number d)
             (expt 10 (length ds)))
          (digits->number ds))]))

  (define (digit->number d)
    (match d
      [#\0 0]
      [#\1 1]
      [#\2 2]
      [#\3 3]
      [#\4 4]
      [#\5 5]
      [#\6 6]
      [#\7 7]
      [#\8 8]
      [#\9 9]))

  ;;--------------------------------------------------------------------------------------------------
  (define (modulo n m)
    (if (< n m)
        n
        (modulo (- n m) m)))
    
  ;;--------------------------------------------------------------------------------------------------
  ;; computes nth prim

  (stream-get (primes) (sub1 (read-number))))
