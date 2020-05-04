#lang racket
(let ((y (ccall c_fun))) ((lambda (x) y) 11))
