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


(define m1-date "Oct 11")
(define m2-date "Nov 20")
(define midterm-hours "24") ; for summer
(define final-date "TBD")
(define elms-url "https://umd.instructure.com/courses/1350066")


(define racket-version "8.10")

(define staff
  (list #;(list "William Wegand" "wwegand@terpmail.umd.edu" "3:00-4:00PM MTWThF")
        (list "Pierce Darragh" "pdarragh@umd.edu")
	(list "Fuxiao Liu" "fl3es@umd.edu")
	(list "Vivian Chen" "vchen8@terpmail.umd.edu")
	(list "Ian Morrill" "imorrill@terpmail.umd.edu")
	(list "Matthew Schneider" "mgl@umd.edu")
	(list "Rhea Jajodia" "rjajodia@terpmail.umd.edu")
	(list "Syed Zaidi" "szaidi@umd.edu")
	(list "William Wegand" "wfweg@verizon.net")
	(list "Wilson Smith" "smith@umd.edu")
	(list "Yuhwan Lee" "ylee9251@terpmail.umd.edu")
        ))


(define lecture-schedule1 "MW, 2:00-3:15pm")
(define lecture-schedule2 "MW, 3:30-4:45pm")

(define classroom1 "BRB 1101")
(define classroom2 "IRB 0318")

;(define discord "TBD")
(define piazza "https://piazza.com/class/llwlfinnxr63c0/")
(define gradescope "https://www.gradescope.com/courses/593113")

(define feedback "https://docs.google.com/forms/d/e/1FAIpQLSc80xQELhHb_Ef-tn0DkpH2b6pYadQiT3aYSEJFNqEqBjzdGg/viewform?usp=sf_link")