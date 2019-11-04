#lang scribble/manual
@title[#:tag "Assignment 7" #:style 'unnumbered]{Assignment 7: Symbols, interning, and gensym}

@(require (for-label (except-in racket ...)))
@;(require "../notes/fraud-plus/semantics.rkt")
@;(require redex/pict)

@(require "../notes/ev.rkt")

@bold{Due: Tues, Nov 12, 11:59PM}

@(define repo "https://classroom.github.com/a/5UM2CXXa")

The goal of this assignment is to (1) implement symbols and the
@racket[eq?] primitive operation, (2) to implement symbol interning by
program transformation.

Assignment repository:
@centered{@link[repo repo]}

You are given a repository with a starter compiler similar to the
@seclink["Loot"]{Loot} language we studied in class.

The given code also implements all the ``plus'' features we've
developed in past assignments.

@section[#:tag-prefix "a7-" #:style 'unnumbered]{Symbols}

Your first task is to implement symbols for the Loot+ language.
You've used symbols extensively throughout the semester, so their use
should be familiar to you.  A symbol evaluates to itself:

@ex[
'foo
]

Your first task is to implement a symbol data type.  The given code
includes syntax checking for programs that may contain symbols and
run-time support for printing symbols.  The compiler has been stubbed
for compiling symbols.  You will need to implement
@racket[compile-symbol] in @tt{compile.rkt}.

A symbol can be represented much like a string: as a continuous
sequence of characters in memory, along with a length field.  The type
tag is different, since strings and symbols should be disjoint data
types.

Once you implement @racket[compile-symbol], you should be able to
write programs that contain symbols.

@section[#:tag-prefix "a7-" #:style 'unnumbered]{Pointer equality}

Your next task is to implement the @racket[eq?] primitive operation,
which compares two values for pointer equality.  Immediate values
(characters, integers, booleans, empty list, etc.) should be
pointer-equal to values that are ``the same.''  So for example:

@ex[
(eq? '() '())
(eq? 5 5)
(eq? #\a #\a)
(eq? #\t #\t)
]

On the other hand, values that are allocated in memory such as boxes,
pairs, procedures, etc., are only @racket[eq?] to each other if they
are allocated to the same location in memory.  So for example, the
following could all produce @racket[#f]:

@ex[
(eq? (λ (x) x) (λ (x) x))
(eq? (cons 1 2) (cons 1 2))
(eq? (box 1) (box 1))
]

However these must be produce @racket[#t]:

@ex[
(let ((x (λ (x) x)))
  (eq? x x))
(let ((x (cons 1 2)))
  (eq? x x))
(let ((x (box 1)))
  (eq? x x))
]

Applying @racket[eq?] to any two values from disjoint data types
should produce @racket[#f]:

@ex[
(eq? 0 #f)
(eq? #\a "a")
(eq? '() #t)
(eq? 'fred "fred")
]

The given compiler is stubbed for the @racket[eq?] primitive.  You
must implement @racket[compile-eq?].

@section[#:tag-prefix "a7-" #:style 'unnumbered]{Interning symbols}

One thing you may notice at this point is that because symbols are
allocated in memory, the behavior @racket[eq?] with your compiler
differs from Racket's behavior.

In Racket, two symbols which are written the same way in a given
program are @racket[eq?] to each other.

@ex[
(eq? 'x 'x)
]

But your compiler will (probably) produce @racket[#f].

The problem is that Racket ``interns'' symbols, meaning that all
occurrences of a symbol are allocated to the same memory location.
(Languages like Java also do this with string literals.)

Extend your compiler so that @racket[eq?] behaves correctly on
symbols.  Note, you should @emph{not change the way @racket[eq?]
works}, rather you should change how symbols are handled by the
compiler.

The most effective way to implement symbol interning is to apply a
program transformation to the given program to compile.  This
transformation should replace multiple occurrences of the same symbol
with a variable that is bound to that symbol, and that symbol should
be allocated exactly once.

So for example,

@racketblock[
(eq? 'fred 'fred)
]

could be transformed to:

@racket[
(let ((x 'fred))
  (eq? x x))
]

The latter should result in @racket[#t] since the @racket['fred]
symbol is allocated exactly once.

The compiler uses a @racket[intern-symbols] function, which does
nothing in the given code, but should be re-defined to perform the
symbol interning program transformation.  Note: you probably want to
define a few helper functions to make @racket[intern-symbols] work.

@section[#:tag-prefix "a7-" #:style 'unnumbered]{Generating symbols}

Finally, implement the @racket[gensym] primitive, which generates a
symbol distinct from all other symbols.

To keep things simple, you should implement the nullary version of
@racket[gensym], i.e. it should take zero arguments and produce a new
symbol.

The following program should always produce @racket[#f]:

@ex[
(eq? (gensym) (gensym))
]

But the following should always produce @racket[#t]:


@ex[
(let ((x (gensym)))
  (eq? x x))
]

Note: Racket's @racket[gensym] will generate a new name for a symbol,
usually something like @racket['g123456], where each successive call
to @racket[gensym] will produce @racket['g123457], @racket['g123458],
@racket['g123459], etc.  Yours does not have to do this (although it's
fine if it does).  All that matters is that @racket[gensym] produces a
symbol that is not @racket[eq?] to any other symbol but itself.

@section[#:tag-prefix "a7-" #:style 'unnumbered]{Bonus}

Should you find yourself having completed the assignment with time to
spare, you could try implementing @racket[compile-tail-apply], which
compiles uses of @racket[apply] that appear in tail position.  It is
currently defined to use the non-tail-call code generator, which means
@racket[apply] does not make a proper tail call.

Keep in mind that this language, the subexpression of @racket[apply]
are arbitrary expressions: @racket[(apply _e0 _e1)] and that
@racket[_e0] may evaluate to a closure, i.e. a function with a saved
environment.  Moreover, the function may have been defined to have
variable arity.  All of these issues will conspire to make tail calls
with @racket[apply] tricky to get right.

This isn't worth any credit, but you might learn something.

@section[#:tag-prefix "a7-" #:style 'unnumbered]{Testing}

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

@bold{There is separate a repository for tests!} When you push your
code, Travis will automatically run your code against the tests.  If
you would like to run the tests locally, clone the following
repository into the directory that contains your compiler and run
@tt{raco test .} to test everything:

@centered{@tt{https://github.com/cmsc430/assign07-test.git}}

This repository will evolve as the week goes on, but any time there's
a significant update it will be announced on Piazza.

@section[#:tag-prefix "a7-" #:style 'unnumbered]{Submitting}

Pushing your local repository to github ``submits'' your work.  We
will grade the latest submission that occurs before the deadline.

