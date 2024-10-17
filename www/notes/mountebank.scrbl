#lang scribble/manual

@(require (for-label (except-in racket compile ...) a86))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  "utils.rkt"
	  "ev.rkt"
	  "../fancyverb.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86))
@(ev `(current-directory ,(path->string (build-path langs "mountebank"))))
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))
@(void (ev '(current-objs '("runtime.o"))))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '("interp.rkt" "compile.rkt" "compile-expr.rkt" "compile-literals.rkt" "compile-datum.rkt" "utils.rkt" "ast.rkt" "parse.rkt" "types.rkt"))

@(define this-lang "Mountebank")

@title[#:tag this-lang]{@|this-lang|: quote and compound static data}

@src-code[this-lang]

@table-of-contents[]

@section[#:tag-prefix "mountebank"]{Quote}

One of the distinguishing features of the Lisp family of languages is
the @racket[quote] form, abbreviated @tt{'}, which is a notation for
writing down literal s-expressions.

Recall that an S-Expression is:

@#reader scribble/comment-reader
(racketblock
;; type S-Expr =
;;             | Boolean
;;             | Number
;;             | Character
;;             | String
;;             | Symbol
;;             | Empty
;;             | (Boxof S-Expr)
;;             | (Pairof S-Expr S-Expr)
;;             | (Vectorof S-Expr)
)

Using quotes, we can write down a literal s-expression such as:

@ex[
'#t
'#f
'9
'#\f
'f
'()
'#&7
'(1 . 2)
'#(1 2 3)
'(a b ((c) #(d)))
]

The grammar of things that can be written down inside of a
@racket[quote] are:

@#reader scribble/comment-reader
(racketblock
;; Datum d ::= #t
;;          |  #f
;;          |  n     where n is a Number literal
;;          |  c     where c is a Character literal
;;          |  s     where s is a String literal
;;          |  s     where s is a Symbol literal
;;          |  ()
;;          |  #&d
;;          |  (d . d)
;;          | #(d ...)
)

At a first level of understanding, it's possible to understand
@racket[quote] by rewriting to ``push'' the quote in as far as
possible.

Some things are ``self-quoting,'' e.g. booleans, characters, strings,
numbers, boxes, and vectors; thus @racket[#t] and @racket['#t] are the
same.  When we have a @racket[quote] around a self-quoting datum, we
can delete it.

Other datums like symbols and the empty list cannot push the
@racket[quote] in further, so we have @racket['()] and @racket['fred]
as literals for the empty list and the symbol @racket[fred],
respectively.

Pairs, boxes, and vectors are @bold{compound datums}.  We can
understand @racket[#&_d] as @racket[(box '_d)] and @racket['(_d1
. _d2)] as @racket[(cons '_d1 '_d2)] and @racket[#(_d ...)] as
@racket[(vector '_d ...)].

We've been using the @racket[quote]-notation from the beginning of the
course so it should be familiar by now.

One of the key things about @racket[quote] is that we can go from the
concrete syntax of an expression as a piece of code, e.g. @racket[(if
(zero? x) 0 (+ x (tri (sub1 x))))], to @emph{a representation of that
expression} as a piece of data by prepending a single character;
@tt{'}, e.g. @racket['(if (zero? x) 0 (+ x (tri (sub1 x))))].

We've relied on this in the front-end of our compiler and interpreter
to parse programs by first calling @racket[read], which reads a single
datum:

@ex[
(with-input-from-string
  "(if (zero? x) 0 (+ x (tri (sub1 x))))"
  read)
]

Let us now add fully support for @racket[quote] to our language.
Let's call it @bold{Mountebank}.

We will change the AST definition for Mountebank to add a
@racket[Quote] constructor, which contains a datum.  Since
@racket[(Str _s)] and @racket[(Quote _s)] where @racket[_s] is a
string are redundant, we remove all of the literal constructors.

Here is the new AST definition:

@filebox-include[codeblock mountebank "ast.rkt"]

The parser is updated to parse things like booleans, numbers, etc. as
@racket[Quote] nodes now and also to support the ability to write
arbitrary datum value under a quote:

@filebox-include[codeblock mountebank "parse.rkt"]


@section[#:tag-prefix "mountebank"]{Quotes are constants}

One thing that the ``pushing quote'' in understanding of
@racket[quote] misses is that a @racket[quote] expression produces a
constant, unlike the use of operations to construct an equivalent
value.

Using @racket[eq?] we can observe the difference.  Recall that
@racket['(1 . 2)] produces a value equivalent to @racket[(cons 1 2)];
however @racket['(1 . 2)] is a constant, whereas @racket[(cons 1 2)]
dynamically allocates memory to represent the pair.

We can see difference here:

@ex[
(define (f) '(1 . 2))
(define (g) (cons 1 2))
(eq? (f) (f))
(eq? (g) (g))
]

Note, this does not mean that all @racket[quote]s are interned
(although some members of the Lisp and Scheme family do this):

@ex[
(define (f) '(1 . 2))
(define (g) '(1 . 2))
(eq? (f) (g))
]

On the other hand, it's important to note that strings and symbols
that appear in @racket[quote]d datums are interned as usual:

@ex[
(define (f) '("first" . second))
(define (g) '("first" . second))
(eq? (car (f)) (car (g)))
(eq? (cdr (f)) (cdr (g)))
(eq? (f) (g))
]



@section[#:tag-prefix "mountebank"]{Interpreting quote}

Interpreting a quoted datum is trivial---it evaluates to the datum
itself:

@filebox-include[codeblock mountebank "interp.rkt"]

The proper treatment of datums as constants is inherited from Racket,
so our interpreter does the right thing on these examples:

@ex[
(define (run . p)
  (interp (parse p)))

(run '(define (f) (cons 1 2))
     '(eq? (f) (f)))

(run '(define (f) '(1 . 2))
     '(eq? (f) (f)))
]

@section[#:tag-prefix "mountebank"]{Compiling quote}

Compiling @racket[quote] is not difficult.  We've seen all the
necessary pieces already.  The key things to observe are:

@itemlist[

@item{a compound quoted datum should be statically allocated,}

@item{strings and symbols that appear in datums should be interned.}

]

The latter is achieved by extending the @racket[literals] function
from Mug to traverse the datum in a @racket[quote] to extract any
string or symbol occurrences.

@filebox-include[codeblock mountebank "compile-literals.rkt"]

The static allocation of compound datums is achieved use the same
static memory allocation mechanism we saw when allocating the string
data of strings and symbols.

Here's how datums are compiled:

@itemlist[

@item{strings are compiled to the tagged address of their string data,}

@item{symbols are compiled to the tagged address of their string data,}

@item{atoms are compiled to their bit represetation, and}

@item{compound datums are compiled to a static chunk of memory to
contain its data and a tagged address of that memory.}

]

Let's see some examples:

@ex[

(compile-datum 0)

(compile-datum #f)

(compile-datum 'fred)

(compile-datum "fred")

(compile-datum '(1 . 2))

]

In the last example, you'll notice we get a @racket[(Data)] section
that includes 2 words of memory; the first contains the bit
representation of @racket[2], i.e. the @racket[cdr] of the pair, and
the second contains the bit representation of @racket[1], i.e. the
@racket[car] of the pair.  After the @racket[(Data)] section, we
switch back to @racket[(Text)] mode with an instruction to load the
address of the statically allocated pair, appropriately tagged.

Datums can be built up arbitrarily large, so in order to compound
datums, we need to recursive traverse their structure to emit the
static data section of their construction.  Here's a larger example:

@ex[
(compile-datum '((3) fred #(x y z) (("wilma"))))
]

Notice that every compound datum has its own label and when they are
contained within other compound datums, we get references,
appropriately tagged, to those labels.

Here is a simple example of a nested datum: a box containing a box
containing zero.

@ex[
(compile-datum '#&#&0)
]

The data section starts with a label and word for the outer box.  The
word contains a tagged reference to the inner box, which is defined
immediately below as a label and word.  That word contains @racket[0].
In the text section there is a single instruction to load the tagged
address of the outer box into @racket['rax].

Here is the complete code for @racket[compile-datum]:

@filebox-include[codeblock mountebank "compile-datum.rkt"]

Now we've succsefully implemented @racket[quote] and can confirm are
examples behave as expected:

@ex[
(current-objs '("runtime.o"))
(define (run . p)
  (bits->value (asm-interp (compile (parse p)))))

(run '#t)
(run ''#t)
(run ''(1 . 2))
(run ''(1 fred #("wilma")))
(run '(define (f) '(1 . 2))
     '(eq? (f) (f)))
(run '(define (f) '("fred" . wilma))
     '(define (g) '("fred" . wilma))
     '(eq? (car (f)) (car (g))))
(run '(define (f) '("fred" . wilma))
     '(define (g) '("fred" . wilma))
     '(eq? (cdr (f)) (cdr (g))))
]

@section[#:tag-prefix "mountebank"]{Getting Meta}

It's worth taking stock of the kind of programs we can now write.
Since @racket[quote] let's us write down data that looks an awful lot
like programs, we can start to write programs that operate over this
kind of data in a way that may seem familiar.

For example, here's a program that interprets a little language that
has elements of the ones we've been building:

@filebox-include[codeblock mountebank "simple-interp.rkt"]


Now of course this is a Racket program, which we can run.  Running it
will run the interpreter we defined on the input program, computing
the 36th triangular number:

@(define (shellbox . s)
   (parameterize ([current-directory (build-path langs "mountebank")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@shellbox[
"racket simple-interp.rkt"
]

But of course, this is also a Mountebank program!  So we can interpret
it with our Mountenank interpreter:

@shellbox[
"racket -t interp-file.rkt -m simple-interp.rkt"
]

And since it's a Mountebank program, we can also compile it and then
running the resulting executable:

@shellbox[
"make simple-interp.run"
"./simple-interp.run"
]

We are moving ever closer to the point where our compiler can compile
the source code of itself.