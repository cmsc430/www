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


(define m1-date "10/4")
(define m2-date "11/1")
(define final-date "12/17")
(define elms-url "https://umd.instructure.com/courses/1328554")

(define racket-version "8.6")

(define staff
  (list (list "Pierce Darragh"  "pdarragh@umd.edu" "M, 1PM-2PM; T, Th, 11AM-12PM AVW 4160")
        (list "Dhruv Maniktala" "dmanikt@umd.edu"  "T, W, Th, 10AM-11AM, Virtual")
        (list "Chris Maxey"     "cmaxey@umd.edu"   "F, 12PM-3PM AVW 4160")
        (list "Deena Postol"    "dpostol@umd.edu"  "M, 2PM-5PM AVW 4160")
        (list "Matvey Stepanov" "mpstepan@umd.edu" "W, 1PM-4PM AVW 4160")))




(define lecture-schedule "Tuesday & Thursday, 2:00pm - 3:15pm, CSI 1115")

(define discord "https://discord.gg/cX5vASt8Tp")

(define gradescope "https://www.gradescope.com/courses/433916")
