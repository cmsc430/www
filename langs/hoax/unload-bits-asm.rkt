#lang racket
(provide unload/free unload-value)
(require "types.rkt"
         ffi/unsafe)

;; Answer* -> Answer
(define (unload/free a)
  (match a
    ['err 'err]
    [(cons h v) (begin0 (unload-value v)
                        (free h))]))

;; Value* -> Value
(define (unload-value v)
  (match v
    [(? imm-bits?) (bits->value v)]
    [(? box-bits? i)
     (box (unload-value (heap-ref i)))]
    [(? cons-bits? i)
     (cons (unload-value (heap-ref (+ i 8)))
           (unload-value (heap-ref i)))]
    [(? vect-bits? i)
     (if (zero? (untag i))
         (vector)
         (build-vector (heap-ref i)
                       (lambda (j)
                         (unload-value (heap-ref (+ i (* 8 (add1 j))))))))]
    [(? str-bits? i)
     (if (zero? (untag i))
         (string)
         (build-string (heap-ref i)
                       (lambda (j)
                         (char-ref (+ i 8) j))))]))

(define (untag i)
  (arithmetic-shift (arithmetic-shift i (- (integer-length ptr-mask)))
                    (integer-length ptr-mask)))

(define (heap-ref i)
  (ptr-ref (cast (untag i) _int64 _pointer) _uint64))

(define (char-ref i j)
  (integer->char (ptr-ref (cast (untag i) _int64 _pointer) _uint32 j)))
