#lang scribble/manual
@title[#:tag "Assignment 5" #:style 'unnumbered]{Assignment 5: A Heap of Characters}

@(require (for-label (except-in racket ...)))
@(require "../notes/fraud-plus/semantics.rkt")
@(require redex/pict)

@(require "../notes/ev.rkt")

@bold{Due: Friday, Oct 30, 11:59PM EDT}

@(define repo "https://classroom.github.com/a/fqM9KeO1")

The goal of this assignment is to extend a compiler with data types
that require memory allocation and dereferencing.

Assignment repository:
@centered{@link[repo repo]}

You are given a repository with a starter compiler similar to the
@seclink["Hustle"]{Hustle} language we studied in class.  You are tasked
with:

@itemlist[

@item{incorporating the Con+ features you added in
@seclink["Assignment 3"]{Assignment 3},}

@item{incorporating the Fraud+ features you added in
@seclink["Assignment 4"]{Assignment 4},}

@item{extending the language to include a string data type,}

@item{implementing a number of primitives,}

]

As usual, the code in @tt{ast.rkt} will need to be studied in order to
understand the structure of the AST and how to traverse/operate on
Hustle+ programs.

@section[#:tag-prefix "a5-" #:style 'unnumbered]{From Fraud+ to Hustle+}

Implement all the features of Con+ and Fraud+, extended to Hustle+.

In order to get all the points for this section of the assignment you will
need to modify the following files:

@itemlist[

@item{@tt{compile.rkt}}

@item{@tt{syntax.rkt}}

]

@section[#:tag-prefix "a5-" #:style 'unnumbered]{Strung out}

In the last assignment, you implemented a character data type for
representing single letters.  In this assignment, you will implement a
String data type for representing arbitrarily long sequences of
characters.

Strings are disjoint from all other data types and are essentially a
fixed-size array of characters.  Literal strings are written by
enclosing the characters within the string in double quotes (@tt{"}).
Strings can include double quotes by using the escape sequence
@tt{\"}.

You must add the following operations to Hustle+:

@itemlist[
@item{@code[#:lang "racket"]{string? ; Any -> Boolean}, which determines if its argument is a string.}
@item{@code[#:lang "racket"]{string-ref ; String Natural -> Char}, which 
extracts the character at the given index (using 0-based counting).  An error is signalled if 
the index is out of bounds for the given string.}
@item{@code[#:lang "racket"]{string-length ; String -> Natural}, which computes the length of the string.}
@item{@code[#:lang "racket"]{make-string ; Natural Char -> Natural}, which constructs a string of the given
length, filled with the given character.}
]

The run-time system has been updated to account for a string type.  It
assumes a representation where the length of the string is stored in
memory, followed by the characters of the string, in order.  You are
free to change the representation if you'd like, but you will have to
update the run-time system to properly print strings.  Otherwise, no
changes to the run-time system should be necessary.

If you want to understand the details of how strings are implemented in
the run-time system. See the function @tt{print_string()}
in @tt{main.c}.

In order to get all the points for this section of the assignment you will
need to modify the following files:

@itemlist[

@item{@tt{compile.rkt}}

@item{@tt{syntax.rkt}}

]

@section[#:tag-prefix "a5-" #:style 'unnumbered]{More operations}

Add the following operations to the Hustle+ language:

@itemize[

@item{@code[#:lang "racket"]{box? ; Any -> Boolean}, which determines
if a value is a box.}

@item{@code[#:lang "racket"]{empty? ; Any -> Boolean}, which
determines if a value is the empty list.}

@item{@code[#:lang "racket"]{cons? ; Any -> Boolean}, which determines
if a value is a pair.}

@item{@code[#:lang "racket"]{= ; Number Number -> Boolean}, which determines if
two numbers are equal.}

@item{@code[#:lang "racket"]{< ; Number Number -> Boolean}, which determines if
the first number is less than the second.}

@item{@code[#:lang "racket"]{<= ; Number Number -> Boolean}, which determines if
the first number is less than or equal to the second.}

@item{@code[#:lang "racket"]{char=? ; Number Number -> Boolean}, which determines if
two characters are equal.}

@item{@code[#:lang "racket"]{boolean=? ; Boolean Boolean -> Boolean}, which determines if
two booleans are equal.}

]

Tests for these primitives have not been provided in @tt{test/compile.rkt},
therefore you will need to write appropriate tests for these primitives.

In order to get all the points for this section of the assignment you will
need to modify the following files:

@itemlist[

@item{@tt{compile.rkt}}

@item{@tt{syntax.rkt}}

@item{@tt{test/compile.rkt}}

]

@section[#:tag-prefix "a5-" #:style 'unnumbered]{Extending the Parser}

You should not need to make any changes to @tt{lex.rkt} or to @tt{parse.rkt},
but you do need to understand the structure of the resulting AST, which is why
the information below is provided. The grammar, along with the definitions and
utility functions in @tt{ast.rkt} provide all the information necessary to
work with the AST in implementing the new features of Hustle+.

The parser has been extended @emph{for you} from the Fraud+ parser for the
Hustle+ language based on the following grammar:

@verbatim{
<expr> ::= integer
        |  character
        |  boolean
	|  variable
        |  string
        |  empty
        |  ( <compound> )
	|  [ <compound> ]

<compound> ::= <prim1> <expr>
            |  <prim2> <expr> <expr>
            |  - <expr> <maybe-expr>
            |  if <expr> <expr> <expr>
            |  cond <clause>* <else>
	    |  let <bindings> <expr>

<prim1> ::= add1 | sub1 | abs | zero? | integer->char | char->integer
         |  char? | integer? | boolean? | string? | box? | empty? | cons?
         |  box | unbox | car | cdr | string-length

<prim2> ::= make-string | string-ref | = | < | <= 
         |  char=? | boolean=? | + | cons

<maybe-expr> ::= 
              |  <expr>

<clause> ::= ( <expr> <expr> )
          |  [ <expr> <expr> ]

<else> ::= ( else <expr> )
        |  [ else <expr> ]

<bindings> ::= ( <binding>* )
            |  [ <binding>* ]

<binding> ::= ( variable <expr> )
           |  [ variable <expr> ]
}

There is a lexer given to you in @tt{lex.rkt}, which provides two
functions: @racket[lex-string] and @racket[lex-port], which consume a
string or an input port, respectively, and produce a list of tokens,
which are defined as follows (only the new parts are shown):

@#reader scribble/comment-reader
(racketblock
; type Token =
; ...
; | '()
; | String

; type Prim = Prim1 | Prim2 | '-

; type Prim1 =
; | 'add1
; | 'sub1
; | 'zero?
; | 'abs
; | 'integer->char
; | 'char->integer
; | 'char?
; | 'boolean?
; | 'integer?
; | 'string?
; | 'box?
; | 'empty?
; | 'cons?
; | 'box
; | 'unbox
; | 'car
; | 'cdr
; | 'string-length

; type Prim2 =
; | 'make-string
; | 'string-ref
; | '=
; | '<
; | '<=
; | 'char=?
; | 'boolean=?
; | '+
; | 'cons
)

The lexer will take care of reading the @tt{#lang racket} header and
remove any whitespace.

The code in @tt{parse.rkt} implements the parser which constructs an
AST representing a valid Hustle+ expression, if possible, from a list
of tokens. 

As an example, @racket[parse] should produce
@racket[(prim-e `add1 (list (prim-e `sub1 (list (int-e 7)))))]
if given

@racketblock['(lparen (prim add1) lparen (prim sub1) 7 rparen rparen eof)]

@section[#:tag-prefix "a5-" #:style 'unnumbered]{Testing}

You can test your code in several ways:

@itemlist[

 @item{Using the command line @tt{raco test .} from
  the directory containing the repository to test everything.}

 @item{Using the command line @tt{raco test <file>} to
  test only @tt{<file>}.}

 @item{Pushing to github. You can 
  see test reports at:
  @centered{@link["https://travis-ci.com/cmsc430/"]{
    https://travis-ci.com/cmsc430/}}

  (You will need to be signed in in order see results for your private repo.)}]

Note that only a small number of tests are given to you, so you should
write additional test cases.

@bold{There is separate a repository for tests!}  The testing set-up
is slightly different for this assignment.  There is a whole other
repository that contains tests.  When you push your code, Travis will
automatically run your code against the tests.  If you would like to
run the tests locally, clone the following repository into the
directory that contains your compiler and run @tt{raco test .} to test
everything:

@centered{@tt{https://github.com/cmsc430/assign05-test.git}}

This repository will evolve as the week goes on, but any time there's
a significant update it will be announced on Piazza.

@section[#:tag-prefix "a5-" #:style 'unnumbered]{Submitting}

Pushing your local repository to github ``submits'' your work.  We
will grade the latest submission that occurs before the deadline.

