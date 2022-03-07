#lang racket
(provide (all-defined-out))
(require scribble/core scribble/html-properties scribble/manual) 

(define prof (link "http://jmct.cc/" "José Manuel Calderón Trilla"))
(define prof-email "jmct@umd.edu")
(define prof-initials "JMCT")

(define semester "spring")
(define year "2022")
(define courseno "CMSC 430")

(define IRB "IRB") 
(define AVW "AVW")


(define m1-date "03/12")
(define m2-date "TBD")
(define final-date "TBD")
(define elms-url "TBD")

(define racket-version "8.3")

(define staff
  (list (list "Maniktala Dhruv" "dmanikt@umd.edu" "M/Tu 15:30-16:30 AVW 4160")
        (list "Benjamin Glover Quiring" "bquiring@umd.edu" "W 16:30-18:30 AVW 4160")
        (list "William Chung" "wchung1@terpmail.umd.edu" "Thu 14:00-16:00 AVW 4160")))

(define lecture-schedule "Tuesday & Thursday, 9:30am - 10:45pm, IRB 0318")

(define discord "https://discord.gg/N4cke9v38X")

(define gradescope "https://www.gradescope.com/courses/365197")
