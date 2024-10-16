#lang info
(define test-omit-paths '("notes/fp/sieve.rkt"
                          "notes/agreement/example.rkt"))
(define deps
  (list "base" "rackunit"
        "git+https://github.com/cmsc430/a86.git?path=#main"
        "git+https://github.com/cmsc430/langs.git?path=#main"))
