#lang racket
;; run command line compiler and compare against Racket as refernece implementation
(require rackunit
         "../../test-programs/get-progs.rkt"
         "build-runtime.rkt")
(for-each test-prog (get-progs "jig"))
