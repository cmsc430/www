#lang racket

;; run command line compiler and compare against Racket as refernece implementation
(require rackunit "../../test-programs/get-progs.rkt")
(for-each test-prog (get-progs "iniquity"))
