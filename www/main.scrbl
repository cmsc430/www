#lang scribble/manual
@(require scribble/core
	  scriblib/footnote
          scribble/decode
          scribble/html-properties
	  "defns.rkt"
          "utils.rkt")

@(define (blockquote . strs)
   (make-nested-flow (make-style "blockquote" '(command))
                     (decode-flow strs)))


@(define accessible
   (style #f (list (js-addition "accessibility.js")
                   (attributes '((lang . "en"))))))

@title[#:style accessible @courseno]{: Design and Implementation of Programming Languages}

@image[#:scale 1/2 #:style float-right]{img/wizard.png}

@emph{Fall, @year}

@emph{Lectures: Tuesday & Thursday, 2-3:15pm, CSIC 2117}

@emph{Professor: @link["https://www.cs.umd.edu/~dvanhorn/"]{David Van Horn}}

CMSC 430 is an introduction to compilers.  Its major goal is to arm
students with the ability to design, implement, and extend a
programming language. Throughout the course, students will design and
implement several related languages.


@tabular[#:style 'boxed 
         #:row-properties '(bottom-border ())
	 (list (list @bold{Staff} 'cont 'cont 'cont)
	       (list @bold{Name} @bold{Office} @elem{@bold{E-mail}} @elem{@bold{Hours}})
	       (list @link["https://www.cs.umd.edu/~dvanhorn"]{David Van Horn} @elem{5250 @IRB} "dvanhorn@cs.umd.edu" "TBD")
               (list @link["https://sankhs.com/"]{Sankha Narayan Guria} @elem{TBD @IRB} "sankha@cs.umd.edu" "TBD")
               (list "Tasnim Kabir" @elem{TBD @IRB} "tkabir1@cs.umd.edu" "TBD")
               (list "Ivan Quiles-Rodriguez" @elem{TBD @IRB} "iquiles@umd.edu" "TBD")
               (list "John Kastner" @elem{TBD @IRB} "kastner@umd.edu" "TBD"))]


@bold{Assumptions:} This course assumes you know the material in CMSC
330 and CMSC 216.  In particular, you need to know how to program in a
functional programming language like OCaml and some
familiarity with programming in C and Assembly.

@bold{Disclaimer:} All information on this web page is tentative and
subject to change until the start of the semester.

@;include-section{texts.scrbl}
@include-section{syllabus.scrbl}
@include-section{schedule.scrbl}
@include-section{notes.scrbl}
@include-section{assignments.scrbl}
@include-section{racket.scrbl}

@;include-section{labs.scrbl}

@;{
@margin-note*{@emph{Program design} is the study of systematic
thought, planning, and universally useful problem-solving skills
applied in the setting of programming and computation.}

@courseno is an introduction to computing and programming. Its major
goal is to introduce students to the principles of systematic problem
solving through programming and the basic rules of computation.

@margin-note*{@bold{Caveat:} Note that @courseno is being offered on
a trial basis as an alternative introductory course sequence.  If you
take @courseno you @emph{must} take CMSC 132A as a subsequent course.
This course @emph{will not} prepare you for the traditional CMSC 132
(no "A") course.  However, students who complete the 131A-132A
sequence will be fully prepared for all subsequent courses that list
131-132 as prerequisites.}

By the end of the course, majors in computer science will have a sense
for difference between a programmer and a well-trained software
developer. Students from all majors will have a sense of the
complexities involved in developing solid software (highly useful in
case they ever collaborate with such professionals) and they ought to
be able to use the principles of programming to solve many
non-computational problems in a systematic manner.

Masterful programmers design programs the way Jacques Pépin makes an
omellete: with systematic technique, honed creativity, and a strong
aesthetic (``There's the Wrong Way and Jacques Pépin’s Way,''
@hyperlink["http://www.nytimes.com/2011/10/19/dining/jacques-pepin-demonstrates-cooking-techniques.html"]{New
York Times, Oct. 18, 2011}).

This course exposes students to the fundamental techniques of program
design: ``an approach to the creation of software that relies on
systematic thought, planning, and understanding from the very
beginning, at every stage and for every step''
(@hyperlink["https://htdp.org/2018-01-06/Book/part_preface.html"]{HtDP/2e,
Preface}).  While taking this course will @emph{not} make you a great
programmer, you cannot become a great programmer without mastering
these skills.  More importantly, even if you never program again, a
student of design ``will still pick up universally useful
problem-solving skills, experience a deeply creative activity, and
learn to appreciate a new form of aesthetic.''



@(define (bldg abbr)
   (link (string-append "http://www.umd.edu/CampusMaps/bld_detail.cfm?bld_code=" abbr) abbr))
@(define AVW "AVW")
@(define CSI "CSI")

@tabular[#:style 'boxed 
         #:row-properties '(bottom-border ())
	 (list (list @bold{Staff} 'cont 'cont 'cont)
	       (list @bold{Name} @bold{Office} @elem{@bold{E-mail}} @elem{@bold{Hours}})
	       (list @link["https://www.cs.umd.edu/~dvanhorn"]{David Van Horn} @elem{3439 @AVW} "dvanhorn@cs.umd.edu" "3:30-5:30PM Mon")
         (list "Samuel Barham" @elem{1112 @AVW} "sbarham@cs.umd.edu" "10-11AM Mon, Wed")
         (list "William Daseking" @elem{1112 @AVW} "wdasekin@terpmail.umd.edu" "1-3PM Fri")
         ;(list "Aaron Eline" @elem{1112 @AVW} "aeline@terpmail.umd.edu")
	 ;(list "Alex Hsieh" @elem{1112 @AVW} "alex53632@outlook.com")
         ;(list "Cameron Moy" @elem{1112 @AVW} "camoy@cs.umd.edu")
         (list "Deena Postol" @elem{1112 @AVW} " dpostol@umd.edu" "1-3PM Wed")
	 ;(list "Xinlu Shen" @elem{1112 @AVW} "xinlu.shen@yahoo.com")
         (list "Fikko Soenanta" @elem{1112 @AVW} "fsoenant@terpmail.umd.edu" "10AM-12PM Tues")
         ;(list "Rachael Zehrung" @elem{1112 @AVW} "rzehrung@cs.umd.edu")
         #;(list "Beatrix Tran" @elem{1112 @AVW} "btrix8@terpmail.umd.edu" "1:30-3:30PM Mon"))]


@tabular[#:style 'boxed
         #:sep @hspace[1]
	 #:row-properties '(top)
		 (list (list @bold{Location} @elem{3117 @CSI})
	       (list @bold{Time} @elem{MWF 11:00am--11:50am})
	       (list @bold{Midterm 1} @elem{@m1-date, in class})
	       (list @bold{Midterm 2} @elem{@m2-date, in class})
               (list @bold{Final exam} @elem{@final-date})
	       ;(list @nonbreaking{@bold{Final Exam}} @elem{@link["http://www.registrar.umd.edu/current/registration/exam%20tables%20spring.html"]{Monday, May 16, 10:30-12:30pm}, 1122 CSI})
	       (list @bold{Textbooks} @elem{@link["https://htdp.org/2018-01-06/Book/"]{@emph{How to Design Programs}, 2nd edition}, Felleisen, et al.}))]

@;include-section{syllabus.scrbl}
@;include-section{exams.scrbl}
@;include-section{labs.scrbl}
@;include-section{assignments.scrbl}
@;include-section{notes.scrbl}
@;include-section{style.scrbl}

@;{section[#:style 'unnumbered]{Piazza}

All announcements will be made on @link["http://piazza.com/umd/fall2018/cmsc131a"]{Piazza}.  Please sign-up at the start of the semester.

Emergency announcement such as last-minute class cancelations (which
should not happen often), will also be announced via the university
email system.

@section[#:style 'unnumbered]{Grades Server}

All grades will be posted on the Grades server.

@url{http://grades.cs.umd.edu/}

@section[#:style 'unnumbered]{Submit Server}

@url[(string-append "http://submit.cs.umd.edu/" semester year)]

@;include-section{resources.scrbl}
@include-section{acks.scrbl}

}
}