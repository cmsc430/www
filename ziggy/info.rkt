#lang info
(define version "1.0")
(define collection 'use-pkg-name)
(define compile-omit-paths (list "src"))
(define test-omit-paths (list "src/test"))
(define deps (list "base" "rackunit"
                   "git+https://github.com/dvanhorn/crook.git?path=#main"))

