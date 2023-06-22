#lang info
(define version "1.0")
(define collection 'use-pkg-name)
(define compile-omit-paths (list "src"))
(define deps (list "base" "rackunit" "redex-lib"
                   "git+https://github.com/dvanhorn/crook.git?path=#main"))

