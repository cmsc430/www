#lang racket
(provide (all-defined-out))
(require scribble/core scribble/html-properties scribble/manual) 

(define prof1 (link "https://jmct.cc" "José Manuel Calderón Trilla"))
(define prof1-pronouns "he/him")
(define prof1-email "jmct@cs.umd.edu")
(define prof1-initials "JMCT")

(define prof2 (link "https://www.cs.umd.edu/~dvanhorn/" "David Van Horn"))
(define prof2-pronouns "he/him")
(define prof2-email "dvanhorn@cs.umd.edu")
(define prof2-initials "DVH")

(define semester "fall")
(define year "2023")
(define courseno "CMSC 430")

(define lecture-dates "" #;"May 30 -- July 7, 2023")

(define IRB "IRB") 
(define AVW "AVW")
(define KEY "KEY")


(define m1-date "TBD")
(define m2-date "TBD")
(define midterm-hours "24") ; for summer
(define final-date "TBD")
(define elms-url "TBD")


(define racket-version "8.10")

(define staff
  (list #;(list "William Wegand" "wwegand@terpmail.umd.edu" "3:00-4:00PM MTWThF")
        (list "Pierce Darragh" "pdarragh@umd.edu" "TBD")
	(list "Fuxiao Liu" "fl3es@umd.edu" "TBD")
	(list "Vivian Chen" "vchen8@terpmail.umd.edu" "TBD")
	(list "Ian Morrill" "imorrill@terpmail.umd.edu" "TBD")
	(list "Matthew Schneider" "mgl@umd.edu" "TBD")
	(list "Rhea Jajodia" "rjajodia@terpmail.umd.edu" "TBD")
	(list "Syed Zaidi" "szaidi@umd.edu" "TBD")
	(list "William Wegand" "wfweg@verizon.net" "TBD")
	(list "Wilson Smith" "smith@umd.edu" "TBD")
	(list "Yuhwan Lee" "ylee9251@terpmail.umd.edu" "TBD")
        ))


(define lecture-schedule1 "MW, 2:00-3:15pm")
(define lecture-schedule2 "MW, 3:30-4:45pm")

(define classroom1 "BRB 1101")
(define classroom2 "IRB 0318")

(define discord "TBD")
(define piazza "TBD")
(define gradescope "TBD")
