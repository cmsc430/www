#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@;(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "iniquity" f))))))
@;	   '("interp.rkt" "ast.rkt" "parse.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt"))

@title[#:tag "Juvie"]{Juvie: cleaning up after your mess}

@emph{Many a man fails to become a thinker for the sole reason that his memory is too good.}

@table-of-contents[]

@section[#:tag-prefix "juvie"]{Remembering to Forget}

By combining the chocolate and peanut butter of
@secref{Hustle} and @secref{Iniquity}, we have achieved a truly
powerful programming language. Indeed, we have achieved the
@emph{ultimate} power: Turing completeness. Every computable
function can be written as a program in our language. (To
convince yourself, try thinking of how you could translate
any Turing machine description into a program in your
language.) In principle, there's nothing more to do.

Pragmatically speaking, of course, there's plenty to do.
There's a reason we don't program by defining Turing
machines. But of the first pragmatic issues to grapple with
is also at the heart of the Turing machine abstraction: the
myth of the inifinite tape.

For the purposes of the theory of computation, it's
perfectly reasonable to assume you have an infinitely long
tape that serves as a model of a computer's memory. A Turing
machine never actually looks at the whole tape at once; it
just gets to see a cell at a time. So you don't actually
need to materialize an impossibly long tape. Instead, the
thinking goes, if you need more tape, more tape can be
procurred @emph{as needed}. That same idea is at play in our
language.

The run-time system allocates a fixed-size heap at start-up
and the program is handed the address of the beginning of
the heap. The program ``allocates'' memory by bumping this
address forward. It @emph{never} deallocates. Memory is only
consumed. What happens if we reach the end of the heap? Bad
things. But just like in Turing machines, we could at least
in principle make the heap bigger. You want more tape? Now
is the time to make it.

But does getting to the end of the heap really mean more
memory is needed?

Suppose for example, we had an extremely small heap that was only
capable of holding two words and that we were executing the
following, admittedly silly, program:

@#reader scribble/comment-reader
(racketblock
(let ((y (let ((x (cons 3 4)))
           (+ (car x) (cdr x)))))
   (cons y y))
)

The program will start by allocating two words for a
cons-cell to hold @racket[(cons 3 4)], the compute the sum
of the pair before then trying, and failing to allocate a
cons-cell for @racket[(cons 3 4)].

But it's fairly easy to see that once the inner
@racket[let]-expression is evaluated, that first cons-cell
is unreachable. And yet it will persist in the heap until
the program finishes executing. What if we could get that
memory back?  
