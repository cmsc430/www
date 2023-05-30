#lang scribble/manual
@title[#:tag "Assignment 6" #:style 'unnumbered]{Assignment 6: Squid Game}

@(require (for-label (except-in racket ...)))
@(require "../notes/ev.rkt"
          "../notes/utils.rkt")

@bold{Due: Monday, July 3, 11:59PM EST}

The goal of this assignment is to hone your testing skills.

@section[#:tag-prefix "a6-" #:style 'unnumbered #:tag "game"]{The Game}

The autograder for this assignment includes a collection of compilers
that implement @secref["Assignment 5"] and a reference interpreter.

You must submit a list of programs that will be run on each compiler.
If a compiler produces a result that is inconsistent with the
reference interpreter, it is eliminated.  Your goal is to construct
a set of test programs that eliminate the largest number of compilers.
The player that eliminates the largest number of compilers, wins.

When you submit, choose a name to display on the leaderboard.  It does
not need to be your real name, but please keep it appropriate for this
setting.

After submitting, click "Leaderboard" to see the latest standings.

There are 133 compilers included.  Your score will be the number of
compilers you are able to eliminate, with a maximum score of 100.

We reserve the right to update the reference interpreter and will
announce any changes on Discord.

The following updates have been made since the release:

@itemlist[

@item{The interpreter now checks for integer overflow and crashes when
this happens, thereby making overflow behavior unspecified for the compilers.}

@item{The interpreter now crashes when interpreting unbound variables
instead of producing @racket['err], making unbound variable behavior
unspecified.}

]

Submissions should be written using the following format:

@codeblock|{
#lang info
(define programs
  (list
    '[ (add1 1) ]
    '[ (write-byte 97) ]
    '[ (define (f x) (+ x x)) (f 5) ]))
}|

If you'd like to include a program reads data from the standard input
port, you can add an enties which are two-element lists, where the first
element is a string that is used as the contents of the input port
and the second element is the program, for example:

@codeblock|{
#lang info
(define programs
  (list
    '[ (add1 1) ]
    '[ (write-byte 97) ]
    '[ "abc" [ (read-byte) ]]
    '[ (define (f x) (+ x x)) (f 5) ]))
}|


You may add as many programs as you'd like to the file.


@section[#:tag-prefix "a6-" #:style 'unnumbered]{Submitting}

You should submit on Gradescope. You should a single file named
@tt{info.rkt} that conforms to the format shown above.
