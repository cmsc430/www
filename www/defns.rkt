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


(define m1-date "Thursday, October 14th")
(define m2-date "Tuesday, November 9th")
(define final-date "TBD")
(define elms-url "TBD")

(define racket-version "8.1")

(define staff
  (list (list (link "http://jmct.cc/" "José Manuel Calderón Trilla") "jmct@umd.edu" "-")        
        (list "William Chung" "wchung1@terpmail.umd.edu" "Th 3:30-5:30 Online")
        (list "Justin Frank" "jpfrank@umd.edu" "W 12:00-2:00 AVW 4160")
        (list "Vyas Gupta" "vgupta13@terpmail.umd.edu" "F 1:30-3:30 AVW 4160")))

(define lecture-schedule "Tuesday & Thursday, 2:00pm - 3:15pm, CSI 2117")

(define discord "https://discord.gg/tyumZUEFSk")

(define gradescope "https://www.gradescope.com/courses/303043")
