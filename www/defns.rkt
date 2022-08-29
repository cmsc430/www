#lang racket
(provide (all-defined-out))
(require scribble/core scribble/html-properties scribble/manual) 

(define prof (link "https://www.cs.umd.edu/~dvanhorn/" "David Van Horn"))
(define prof-email "dvanhorn@cs.umd.edu")
(define prof-initials "DVH")

(define semester "fall")
(define year "2022")
(define courseno "CMSC 430")

(define IRB "IRB") 
(define AVW "AVW")


(define m1-date "TBD")
(define m2-date "TBS")
(define final-date "TBD")
(define elms-url "TBD")

(define racket-version "8.4")

(define staff
  (list (list "Pierce Darragh"  "pdarragh@umd.edu" "TBA")
        (list "Chris Maxey"     "cmaxey@umd.edu"   "TBA")
        (list "Deena Postol"    "dpostol@umd.edu"  "TBA")
        (list "Matvey Stepanov" "mpstepan@umd.edu" "TBA")))

(define lecture-schedule "Tuesday & Thursday, 2:00pm - 3:15pm, CSI 1115")

(define discord "https://discord.gg/cX5vASt8Tp")

(define gradescope "https://www.gradescope.com/courses/365197")
