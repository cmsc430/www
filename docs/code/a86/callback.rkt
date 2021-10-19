#lang racket
;; based on racket/draw/unsafe/callback
(provide guard-foreign-escape)
(require ffi/unsafe
         ffi/unsafe/atomic)

(define callback-atomic? (eq? 'chez-scheme (system-type 'vm)))

(define-syntax-rule (guard-foreign-escape e0 e ...)
  (call-guarding-foreign-escape (lambda () e0 e ...)))

(define (call-guarding-foreign-escape thunk)
  (if callback-atomic?
      ((call-with-c-return
        (lambda ()
          (with-handlers ([(lambda (x) #t)
                           (lambda (x)
                             ;; Deliver an exception re-raise after returning back
                             ;; from `call-with-c-return`:
                             (lambda ()
                               (when (in-atomic-mode?)
                                 (end-atomic)) ; error happened during atomic mode
                               ;(enable-interrupts) ; ... with interrupts disabled
                               (void/reference-sink call-with-c-return-box)
                               (raise x)))])
            (let ([vs (call-with-values thunk list)])
              ;; Deliver successful values after returning back from
              ;; `call-with-c-return`:
              (lambda ()
                (void/reference-sink call-with-c-return-box)
                (apply values vs)))))))
      (thunk)))

(define call-with-c-return-box (box #f))

;; `call-with-c-return` looks like a foreign function, due to a cast
;; to and from a callback, so returning from `call-with-c-return` will
;; pop and C frame stacks (via longjmp internally) that were pushed
;; since `call-with-c-return` was called.
(define call-with-c-return
  (and callback-atomic?
       (cast (lambda (thunk) (thunk))
             (_fun #:atomic? #t
                   #:keep call-with-c-return-box
                   _racket -> _racket)
             (_fun _racket -> _racket))))