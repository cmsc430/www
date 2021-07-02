#lang racket
(provide (all-defined-out))
(require scribble/core scribble/html-properties scribble/manual) 

(define prof (link "https://www.cs.umd.edu/~dvanhorn/" "David Van Horn"))
(define prof-email "dvanhorn@cs.umd.edu")

(define semester "fall")
(define year "2021")
(define courseno "CMSC 430")

(define IRB "IRB") 
(define AVW "AVW")


(define m1-date "TDB" #;"October 13th")
(define m2-date "TBD" #;"November 10th")
(define final-date "TBD")
(define elms-url "TBD")

(define racket-version "8.1")

(define staff
  (list (list (link "http://jmct.cc/" "José Manuel Calderón Trilla") "jmct@umd.edu" "-")        
        (list "William Chung" "wchung12326@gmail.com" "TBD")
        (list "Justin Frank" "jpfrank@umd.edu" "TBD")
        (list "Vyas Gupta" "vgupta13@terpmail.umd.edu" "TBD")))

(define lecture-schedule "Tuesday & Thursday, 2:00pm - 3:15pm, CSI 2117")
