#lang racket
(require rackunit)
(check-true (system "make -C .. self-host-test"))