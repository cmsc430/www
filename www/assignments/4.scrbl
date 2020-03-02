#lang scribble/manual
@title[#:tag "Assignment 4" #:style 'unnumbered]{Assignment 4: Let there be Variables, Characters}

@(require (for-label (except-in racket ...)))
@(require "../notes/fraud-plus/semantics.rkt")
@(require redex/pict)

@(require "../notes/ev.rkt")

@bold{Due: TBD}

@(define repo "https://classroom.github.com/a/HuBPd1o9")

The goal of this assignment is to extend a compiler with binding forms
and a character data type.

Assignment repository:
@centered{Will be released soon}
;@centered{@link[repo repo]}

You are given a repository with a starter compiler similar to the
@seclink["Fraud"]{Fraud} language we studied in class.  You are tasked
with:

@itemlist[

@item{incorporating the Con+ features you added in
@seclink["Assignment 3"]{Assignment 3},}

@item{extending the language to include a character data type,}

@item{extending the @racket[let]-binding form of the language to bind
any number of variables, and}

@item{updating the parser to work for Fraud+.}

]

@section[#:tag-prefix "a4-" #:style 'unnumbered]{From Con+ to Fraud+}

Implement the @racket[abs] and unary @racket[-] operations and the
@racket[cond] form from @seclink["Assignment 3"]{Con+}.  You can start
from your previous code, but you will need to update it to work for
Fraud+.

In particular, functions should signal an error when applied to the
wrong type of argument and your @racket[cond] form should work with
@emph{arbitrary} question expressions.  In other words, @racket[cond]
should work like @racket[if] in @seclink["Dupe"]{Dupe}.

The formal semantics of @racket[cond] are defined as:

@(define ((rewrite s) lws)
   (define lhs (list-ref lws 2))
   (define rhs (list-ref lws 3))
   (list "" lhs (string-append " " (symbol->string s) " ") rhs ""))

@(require (only-in racket add-between))
@(define-syntax-rule (show-judgment name i j)
   (with-unquote-rewriter
      (lambda (lw)
        (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (lw-column lw) (lw-column-span lw)))
      (with-compound-rewriters (['+ (rewrite '+)]
                                ['- (rewrite '–)]
                                ['= (rewrite '=)]
				['!= (rewrite '≠)])
        (apply centered
	   (add-between 
             (build-list (- j i)
	                 (λ (n) (begin (judgment-form-cases (list (+ n i)))
	                               (render-judgment-form name))))
             (hspace 4))))))

@(show-judgment 𝑭-𝒆𝒏𝒗 0 1)
@(show-judgment 𝑭-𝒆𝒏𝒗 1 2)
@(show-judgment 𝑭-𝒆𝒏𝒗 2 3)


The following files have already been updated for you:
@itemlist[
@item{@tt{ast.rkt}}
@item{@tt{syntax.rkt}}
@item{@tt{interp.rkt}}
]

You will need to modify @tt{compile.rkt} to correctly implement these
features.


@section[#:tag-prefix "a4-" #:style 'unnumbered]{Adding a Bit of Character}

Racket has a Character data type for representing single letters.  A
Racket character can represent any of the 1,114,112 Unicode
@link["http://unicode.org/glossary/#code_point"]{code points}.

The way a character is most often written is an octothorp, followed by
a backslash, followed by the character itself.  So for example the
character @tt{a} is written @racket[#\a].  The character @tt{λ} is
written @racket[#\λ].  The character @tt{文} is written @racket[#\文].

A character can be converted to an integer and @emph{vice versa}:

@ex[
(char->integer #\a)
(char->integer #\λ)
(char->integer #\文)
(integer->char 97)
(integer->char 955)
(integer->char 25991)
]

However, integers in the range of valid code points are acceptable to
@racket[integer->char] and using any other integer will produce an
error:

@ex[
(eval:error (integer->char -1))
(eval:error (integer->char 55296))
]

There are a few other ways to write characters (see the Racket
@link["https://docs.racket-lang.org/reference/reader.html#%28part._parse-character%29"]{Reference}
for the details), but you don't have to worry much about this since
the lexer takes care of reading characters in all their different
forms and the run-time system given to you takes care of printing
them.

Your job is extend the compiler to handle the compilation of
characters and implement the operations @racket[integer->char],
@racket[char->integer], and @racket[char?].  The operations should
work as in Racket and should signal an error (i.e. @racket['err])
whenever Racket produces an error.  While you're at it, implement
the predicates @racket[integer?] and @racket[boolean?], too.

The following files have already been updated for you:

@itemlist[

@item{@tt{ast.rkt}}

@item{@tt{syntax.rkt}}

@item{@tt{interp.rkt}}

@item{@tt{main.c}}

]

You will need to modify @tt{compile.rkt} to correctly implement these
features.  Note that you must use the same representation of
characters as used in the run-time system and should not change
@tt{main.c}.

@section[#:tag-prefix "a4-" #:style 'unnumbered]{Generalizing Let}

The Fraud language has a let form that binds a single variable in the
scope of some expression.  This is a restriction of the more general
form of @racket[let] that binds any number of expressions.  So for
example,

@racketblock[
(let ((x 1) (y 2) (z 3))
  _e)
]

simultaneously binds @racket[x], @racket[y], and @racket[z] in the
scope of @racket[_e].

The syntax of a @racket[let] expression allows any number of binders
to occur, so @racket[(let () _e)] is valid syntax and is equivalent to
@racket[_e].

The binding of each variable is only in scope in the body, @bold{not}
in the right-hand-sides of any of the @racket[let].

For example, @racketblock[(let ((x 1) (y x)) 0)] is a syntax error
because the occurrence of @racket[x] is not bound.

The following files have already been updated for you:

@itemlist[

@item{@tt{ast.rkt}}

@item{@tt{interp.rkt}}

]

Update @tt{syntax.rkt} to define two functions:

@itemize[

@item{@code[#:lang "racket"]{expr? ; Any -> Boolean}, which consumes
anything and determines if it is a well-formed expression, i.e. it
must be an instance of an @tt{Expr} @emph{and} each @racket[let]
expression must bind a distinct set of variables.}

@item{@code[#:lang "racket"]{closed? ; Expr -> Boolean}, which consumes
an @tt{Expr} and determines if it is closed, i.e. every variable
occurrence is bound.}

]

Update @tt{compile.rkt} to correctly compile the generalized form of
@racket[let].  The compiler may assume the input is a closed
expression.

@section[#:tag-prefix "a4-" #:style 'unnumbered]{Extending your Parser}


Extend your Con+ parser for the Fraud+ language based on the following
grammar:

@verbatim{
<expr> ::= integer
        |  character
        |  boolean
	|  variable
        |  ( <compound> )
	|  [ <compound> ]

<compound> ::= <prim> <expr>
            |  if <expr> <expr> <expr>
            |  cond <clause>* <else>
	    |  let <bindings> <expr>

<prim> ::= add1 | sub1 | abs | - | zero? | integer->char | char->integer
        |  char? | integer? | boolean?

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
which are defined as follows:

@margin-note{Note that the @tt{Token} type has changed slightly from
@secref{Assignment 3}: @racket['add1] is now @racket['(prim add1)],
@racket['cond] is now @racket['(keyword cond)], etc.}

@#reader scribble/comment-reader
(racketblock
;; type Token =
;; | Integer
;; | Char
;; | Boolean
;; | `(variable ,Variable)
;; | `(keyword ,Keyword)
;; | `(prim ,Prim)
;; | 'lparen    ;; (
;; | 'rparen    ;; )
;; | 'lsquare   ;; [
;; | 'rsquare   ;; ]
;; | 'eof       ;; end of file

;; type Variable = Symbol (other than 'let, 'cond, etc.)

;; type Keyword =
;; | 'let
;; | 'cond
;; | 'else
;; | 'if

;; type Prim =
;; | 'add1
;; | 'sub1
;; | 'zero?
;; | 'abs
;; | '-
;; | 'integer->char
;; | 'char->integer
;; | 'char?
;; | 'boolean?
;; | 'integer?
)

The lexer will take care of reading the @tt{#lang racket} header and
remove any whitespace.

You must complete the code in @tt{parse.rkt} to implement the parser
which constructs an s-expression representing a valid Fraud+
expression, if possible, from a list of tokens.  The @racket[parse]
function should have the following signature and must be provided by
the module:

@#reader scribble/comment-reader
(racketblock
;; parse : [Listof Token] -> Expr
)

As an example, @racket[parse] should produce @racket['(add1 (sub1 7))]
if given

@racketblock['(lparen (prim add1) lparen (prim sub1) 7 rparen rparen eof)]


You should not need to make any changes to @tt{lex.rkt}.

The given @tt{interp-file.rkt} and @tt{compile-file.rkt} code no
longer use @racket[read], but instead use the parser.  This means you
neither will work until the parser is complete.


@bold{The code you are given includes two(!) implementations of the
Con+ parser.}  One implementation follows the imperative approach; the
other follows the functional approach.

You may extend either, or you may throw out the given code and start
from the code you wrote previously.


@section[#:tag-prefix "a4-" #:style 'unnumbered]{Testing}

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

@bold{We have removed @tt{random.rkt}} and instead provide a
@tt{random-exprs.rkt} module which provides @racket[exprs], a list
of 500 closed expressions.  It is used in the
@tt{test/compile-rand.rkt} file to randomly test compiler correctness.
This should help speed up the testing process since the random
generation is slow.

@section[#:tag-prefix "a4-" #:style 'unnumbered]{Submitting}

Pushing your local repository to github ``submits'' your work.  We
will grade the latest submission that occurs before the deadline.

