#lang racket
(provide interp interp-env)
(require racket/fixnum
         "ast.rkt"
         "env.rkt"
         "types.rkt")

;; type Answer* = Bits | 'err
;; type Bits = Integer (64-bits)
;; type Heap = Bytes

;; Allocate by incrementing first word
(define heap (make-bytes (* 8 10001)))

;; Address Word -> Void
(define (heap-set! a w)
  (for ([i 8])
    (bytes-set! heap (+ a i) (bitwise-bit-field w (* i 8) (* (add1 i) 8)))))

;; Address -> Word
(define (heap-ref a)
  (for/fold ([n 0])
            ([i 8])
    (+ n (arithmetic-shift (bytes-ref heap (+ a i)) (* 8 i)))))

;; Bits -> Answer
(define (bits->answer b)
  (match b
    [(? imm-bits?)
     (bits->value b)]
    [(? cons-bits?)
     (let ((a (bitwise-xor b type-cons)))
       (cons (bits->answer (heap-ref (+ a 8)))
             (bits->answer (heap-ref a))))]
    [(? box-bits?)
     (let ((a (bitwise-xor b type-box)))
       (box (bits->answer (heap-ref a))))]
    [(? vect-bits?)
     (let ((a (bitwise-xor b type-vect)))
       (if (zero? a)
           '#()
           (let* ((n (heap-ref a))
                  (v (make-vector n 0)))
             (define (loop i)
               (let ((b (+ a (* 8 (add1 i)))))
                 (match (- n i)
                   [0 (void)]
                   [_ (vector-set! v i (bits->answer (heap-ref b)))
                      (loop (+ i 1))])))
             (loop 0)
             v)))]
    [(? str-bits?)
     (let ((a (bitwise-xor b type-str)))        
       (if (zero? a)
           ""
           (let* ((n (heap-ref a))
                  (s (make-string n #\_)))
             (define (loop i)
               (let ((b (+ a (* 8 (add1 (quotient i 2))))))
                 (match (- n i)
                   [0 (void)]
                   [1 (string-set! s i (integer->char (heap-ref b)))]
                   [_ (string-set! s i (integer->char (lower (heap-ref b))))
                      (string-set! s (add1 i) (integer->char (upper (heap-ref b))))
                      (loop (+ i 2))])))
             (loop 0)
             s)))]
    [_ (error "bad bits")]))

(define (upper w)
  (arithmetic-shift w (- 32)))
(define (lower w)
  (bitwise-bit-field w 0 32))

;; type CEnv = (Listof (Maybe Id))
;; type Defns = (Listof Defn)
;; type Stack = (Listof Bits)

;; Prog -> Answer
(define (interp p)
  (heap-set! 0 8) ; reset the next available word
  (match p
    [(Prog ds e)
     (with-handlers ([(λ (e) (eq? e 'err)) (λ (e) e)])
       (let-values ([(b s) (interp-env e '() '() ds)])
         (match s
           ['() (bits->answer b)]
           [_ (error "bad stack!")])))]))

;; Expr CEnv Stack Defns -> Bits Stack throws 'err
(define (interp-env e r s ds)
  (match e
    [(Int i)  (values (imm->bits i) s)]
    [(Bool b) (values (imm->bits b) s)]
    [(Char c) (values (imm->bits c) s)]
    [(Eof)    (values (imm->bits eof) s)]
    [(Empty)  (values (imm->bits '()) s)]
    [(Var x)  (values (list-ref s (lookup r x)) s)]
    [(Str "") (values type-str s)]
    [(Str str)
     (let ((a (heap-ref 0))
           (n (string-length str)))
       (heap-set! a (string-length str))
       (heap-set! 0 (+ a (* 8 (+ 1 (quotient n 2) (remainder n 2)))))
       (let ()
         (define (loop cs i)
           (match cs
             ['() (void)]
             [(cons c '())
              (heap-set! (+ a i) (char->integer c))]
             [(cons c1 (cons c2 cs))
              (heap-set! (+ a i) (+ (arithmetic-shift (char->integer c2) 32)
                                    (char->integer c1)))
              (loop cs (+ i 8))]))
         (loop (string->list str) 8))
       (values (bitwise-xor a type-str) s))]
    [(Prim0 p)
     (values (interp-prim0 p) s)] ;; XXX will need stack for collect-garbage
    [(Prim1 p e)
     (let-values ([(v s) (interp-env e r s ds)])
       (values (interp-prim1 p v) s))]    
    [(Prim2 p e1 e2)
     (let-values ([(v1 s1) (interp-env e1 r s ds)])
       (let-values ([(v2 s2) (interp-env e2 (cons #f r) (cons v1 s1) ds)])
         (match s2
           [(cons v1 s)
            (values (interp-prim2 p v1 v2) s)])))]    
    [(Prim3 p e1 e2 e3)
     (let-values ([(v1 s1) (interp-env e1 r s ds)])
       (let-values ([(v2 s2) (interp-env e2 (cons #f r) (cons v1 s1) ds)])
         (let-values ([(v3 s3) (interp-env e3 (cons #f (cons #f r)) (cons v2 s2) ds)])
           (match s3
             [(cons v2 (cons v1 s))
              (values (interp-prim3 p v1 v2 v3) s)]))))]    
    [(If p e1 e2)
     (let-values ([(v s) (interp-env p r s ds)])
       (if (= v val-false)
            (interp-env e2 r s ds)
            (interp-env e1 r s ds)))]
    [(Begin e1 e2)
     (let-values ([(_ s) (interp-env e1 r s ds)])
       (interp-env e2 r s ds))]    
    [(Let x e1 e2)
     (let-values ([(v s) (interp-env e1 r s ds)])
       (let-values ([(v0 s0) (interp-env e2 (cons x r) (cons v s) ds)])
         (values v0 (rest s0))))]

    
    [(App f es)
     (match (interp-env* es r s ds)
       [s
        (match (defns-lookup ds f)
          [(Defn f xs e)
           ; check arity matches
           (if (= (length xs) (length es))
               (let-values ([(v0 s0) (interp-env e (reverse xs) s ds)])
                 (values v0 (drop s0 (length xs))))
               (raise 'err))])])]))

;; (Listof Expr) CEnv Stack Defns -> Stack
(define (interp-env* es r s ds)
  (match es
    ['() s]
    [(cons e es)
     (let-values ([(v s) (interp-env e r s ds)])
       (interp-env* es (cons #f r) (cons v s) ds))]))

;; Defns Symbol -> Defn
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _ _) (eq? f g)])
         ds))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Op0 -> Bits
(define (interp-prim0 p)
  (match p
    ['void (imm->bits (void))]
    ['read-byte (imm->bits (read-byte))]
    ['peek-byte (imm->bits (peek-byte))]
    ['dump-memory-stats #;(dump-memory-stats) (imm->bits (void))]
    ['collect-garbage #;(collect-garbage) (imm->bits (void))]
    ))

;; Op1 Bits -> Bits
(define (interp-prim1 p1 v)
  (match (list p1 v)
    [(list 'add1 (? int-bits?))
     (+ v (imm->bits 1))]
    [(list 'sub1 (? int-bits?))
     (- v (imm->bits 1))]
    [(list 'zero? (? int-bits?))
     (imm->bits (zero? v))]
    [(list 'char? v)
     (imm->bits (char-bits? v))]
    [(list 'char->integer (? char-bits?))
     (arithmetic-shift (arithmetic-shift v (- char-shift)) int-shift)]                  
    [(list 'integer->char (? codepoint-bits?))
     (bitwise-xor (arithmetic-shift (arithmetic-shift v (- int-shift)) char-shift)
                  type-char)]
    [(list 'eof-object? v)
     (imm->bits (= v val-eof))]
    [(list 'write-byte (? byte-bits?))
     (begin (write-byte (bits->value v))
            val-void)]
    [(list 'box v)
     (let ((a (heap-ref 0)))
       (heap-set! a v)
       (heap-set! 0 (+ a 8))
       (bitwise-xor a type-box))]
     
    [(list 'unbox (? box-bits?))
     (heap-ref (bitwise-xor v type-box))]
    [(list 'car (? cons-bits?))
     (heap-ref (+ (bitwise-xor v type-cons) 8))]
    [(list 'cdr (? cons-bits?))
     (heap-ref (bitwise-xor v type-cons))]
    [(list 'empty? v)
     (imm->bits (= v val-empty))]
    [(list 'cons? v)
     (imm->bits (cons-bits? v))]
    [(list 'box? v)
     (imm->bits (box-bits? v))]    
    [(list 'vector? v)
     (imm->bits (vect-bits? v))]
    [(list 'vector-length (? vect-bits?))
     (if (= v type-vect)
         0
         (imm->bits (heap-ref (bitwise-xor v type-vect))))]
    [(list 'string? v)
     (imm->bits (str-bits? v))]
    [(list 'string-length (? str-bits?))
     (if (= v type-str)
         0        
         (imm->bits (heap-ref (bitwise-xor v type-str))))]
    [_ (raise 'err)]))

;; Op2 Value Value -> Answer
(define (interp-prim2 p v1 v2)
  (match (list p v1 v2)
    [(list '+ (? int-bits?) (? int-bits?)) (+ v1 v2)]
    [(list '- (? int-bits?) (? int-bits?)) (- v1 v2)]
    [(list '< (? int-bits?) (? int-bits?)) (imm->bits (< v1 v2))]
    [(list '= (? int-bits?) (? int-bits?)) (imm->bits (= v1 v2))]
    [(list 'cons v1 v2)
     (let ((a (heap-ref 0)))
       (heap-set! a v2)
       (heap-set! (+ a 8) v1)
       (heap-set! 0 (+ a 16))
       (bitwise-xor a type-cons))]     
    [(list 'eq? v1 v2) (imm->bits (= v1 v2))]
    [(list 'make-vector (? int-bits?) _)
     (if (<= 0 v1)
         (if (zero? v1)
             type-vect
             (let ((a (heap-ref 0))
                   (n (bits->value v1)))
               (heap-set! a n)
               (for ([i n])
                 (heap-set! (+ a 8 (* i 8)) v2))
               (heap-set! 0 (+ a 8 (* n 8)))
               (bitwise-xor a type-vect)))
         (raise 'err))]
    [(list 'vector-ref (? vect-bits?) (? int-bits?))
     (let ((a (bitwise-xor v1 type-vect)))       
       (if (zero? a)
           (raise 'err)
           (let ((n (heap-ref a)))
             (if (<= 0 (bits->value v2) (sub1 n))
                 (heap-ref (+ a 8 (* 8 (bits->value v2))))
                 (raise 'err)))))]
    [(list 'make-string (? int-bits?) (? char-bits?))
     (if (<= 0 v1)
         (if (zero? v1)
             type-str
             (let ((a (heap-ref 0))
                   (c (arithmetic-shift v2 (- char-shift))))
               (let ((n (bits->value v1)))
                 (heap-set! a n)
                 (define (loop i)
                   (match (- n i)
                     [0 (void)]
                     [1 (heap-set! (+ a 8 (* (quotient i 2) 8)) c)]
                     [_ (heap-set! (+ a 8 (* (quotient i 2) 8))
                                   (+ c (arithmetic-shift c 32)))
                        (loop (+ i 2))]))
                 (loop 0))
               (bitwise-xor a type-str)))
         (raise 'err))]
    [(list 'string-ref (? str-bits?) (? int-bits?))
     (let ((a (bitwise-xor v1 type-str)))       
       (if (zero? a)
           (raise 'err)
           (let ((n (heap-ref a)))
             (if (<= 0 (bits->value v2) (sub1 n))
                 (let ((w (heap-ref (+ a 8 (* 8 (quotient (bits->value v2) 2))))))
                   (bitwise-xor
                    (arithmetic-shift 
                     (if (even? (bits->value v2))
                         (lower w)
                         (upper w))
                     char-shift)
                    type-char))
                 (raise 'err)))))]
    [_ (raise 'err)]))

;; Op3 Value Value Value -> Answer
(define (interp-prim3 p v1 v2 v3)
  (match (list p v1 v2 v3)
    [(list 'vector-set! (? vect-bits?) (? int-bits?) _)
     (let ((a (bitwise-xor v1 type-vect)))       
       (if (zero? a)
           (raise 'err)
           (let ((n (heap-ref a)))
             (if (<= 0 (bits->value v2) (sub1 n))
                 (begin (heap-set! (+ a 8 (* 8 (bits->value v2))) v3)
                        val-void)
                 (raise 'err)))))]
    [_ (raise 'err)]))

;; Bits -> Boolean
(define (byte-bits? b)
  (and (int-bits? b)
       (<= 0 b (imm->bits 255))))

;; Bits -> Boolean
(define (codepoint-bits? v)
  (and (int-bits? v)
       (or (<= 0 v (imm->bits 55295))
           (<= (imm->bits 57344) v (imm->bits 1114111)))))


(define (lookup r x)
  (match r
    [(cons y r)
     (if (eq? x y)
         0
         (add1 (lookup r x)))]))