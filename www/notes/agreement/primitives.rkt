#lang racket
(provide (all-defined-out))

(define (get-int)
  (let ((in (string->number (read-line (current-input-port) 'any))))
       (if (integer? in)
           in
           (error "error"))))

(define get-int-asm
  `((mov r15 rsp)
    ; align rsp to safest 16-byte aligned spot
    (and rsp -16)
    (call get_int)
    (mov rsp r15)))
