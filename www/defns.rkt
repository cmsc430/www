#lang racket
(provide (all-defined-out))
(require scribble/core scribble/html-properties scribble/manual) 

;(define prof1 (link "https://jmct.cc" "José Manuel Calderón Trilla"))
;(define prof1-pronouns "he/him")
;(define prof1-email "jmct@cs.umd.edu")
;(define prof1-initials "JMCT")

(define prof1 (link "https://www.cs.umd.edu/~dvanhorn/" "David Van Horn"))
(define prof1-pronouns "he/him")
(define prof1-email "dvanhorn@cs.umd.edu")
(define prof1-initials "DVH")

(define semester "spring")
(define year "2024")
(define courseno "CMSC 430")

(define lecture-dates "" #;"May 30 -- July 7, 2023")

(define IRB "IRB") 
(define AVW "AVW")
(define KEY "KEY")


(define m1-date "March 6")
(define m2-date "April 17")
(define midterm-hours "24")
(define final-date "May 14")
(define elms-url "https://umd.instructure.com/courses/1359023")


(define racket-version "8.11")

(define staff
  (list (list "Henry Blanchette" "blancheh@umd.edu")
        (list "Pierce Darragh" "pdarragh@umd.edu")
	(list "Advait Kushe" "akushe@terpmail.umd.edu")
	(list "Deena Postol" "dpostol@umd.edu")
        (list "William Wegand" "wwegand@terpmail.umd.edu")
	(list "Kazi Tasnim Zinat" "kzintas@umd.edu")
	#;(list "Fuxiao Liu" "fl3es@umd.edu")
	#;(list "Vivian Chen" "vchen8@terpmail.umd.edu")
	#;(list "Ian Morrill" "imorrill@terpmail.umd.edu")
	#;(list "Matthew Schneider" "mgl@umd.edu")
	#;(list "Rhea Jajodia" "rjajodia@terpmail.umd.edu")
	#;(list "Syed Zaidi" "szaidi@umd.edu")
	#;(list "William Wegand" "wfweg@verizon.net")
	#;(list "Wilson Smith" "smith@umd.edu")
	#;(list "Yuhwan Lee" "ylee9251@terpmail.umd.edu")
        ))


;(define lecture-schedule1 "MW, 2:00-3:15pm")
(define lecture-schedule1 "MW, 3:30-4:45pm")

(define classroom1 "HJP 0226")

;(define discord "TBD")
(define piazza "https://piazza.com/class/lrs6masma6h2o1/")
(define gradescope "https://www.gradescope.com/courses/723511")

(define feedback "https://docs.google.com/forms/d/e/1FAIpQLSc80xQELhHb_Ef-tn0DkpH2b6pYadQiT3aYSEJFNqEqBjzdGg/viewform?usp=sf_link")
