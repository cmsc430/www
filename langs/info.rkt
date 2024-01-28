#lang info
(define version "1.0")
(define collection 'multi)
(define deps (list "base" "rackunit" "redex-lib"))
(define build-deps
  (list "https://github.com/cmsc430/www.git?path=ziggy#ziggy"))

;; Outlaw is omitted here because it depends on libraries that are a pain
;; to ensure are set up properly and we don't want students to see failing
;; tests at the beginning of the semester, nor do we want to get into
;; setting up libraries only needed in the last week and only if you
;; actually care to run Outlaw.

;; To test outlaw you should do an explicit: raco test -c outlaw
(define test-omit-paths (list "outlaw"))
