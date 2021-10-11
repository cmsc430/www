#lang scribble/manual

@(require (for-label (except-in racket ... compile) a86))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  ; (except-in "../../langs/hustle/semantics.rkt" ext lookup)
          ; (prefix-in sem: (only-in "../../langs/hustle/semantics.rkt" ext lookup))
	  "../fancyverb.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86))
@(ev `(current-directory ,(path->string (build-path notes "hoax"))))
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '("interp.rkt" "compile.rkt" "compile-ops.rkt" "ast.rkt" "parse.rkt" "types.rkt"))

@(define this-lang "Hoax")

@title[#:tag this-lang]{@|this-lang|: vectors, strings, symbols}


@emph{Stupidity, outrage, vanity, cruelty, iniquity, bad faith,
falsehood - we fail to see the whole array when it is facing in the
same direction as we.}

@table-of-contents[]

@section{Array data}

In the @bold{@this-lang} language, we will add an @bold{array
data type} known as vectors.

Vectors are fixed-length arrays with constant-time access and update
of the vector slots, which are numbered from @racket[0] to one less
than the number of slots in the vector.

We extend our definition of values:

@#reader scribble/comment-reader
(racketblock
;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)
;; | (vector Value ...)
)


The new operations include the constructor @racket[(make-vector _e1
_e2)], predicate @racket[(vector? _e0)], accessor @racket[(vector-ref
_e0 _e1)] and mutator @racket[(vector-set! _e0 _e1 _e2)].

These features will operate like their Racket counterparts:
@ex[
(make-vector 3 #t)
(vector? (make-vector 3 #t))
(vector-ref (make-vector 3 #t) 0)
(vector-ref (make-vector 3 #t) 2)
(let ((v (make-vector 3 #t)))
  (begin (vector-set! v 1 #f)
         v))
]

We can model this syntax as an AST data type:

@filebox-include-fake[codeblock "hoax/ast.rkt"]{
#lang racket
;; type Expr = ...
;;           | (Prim3 Op3 Expr Expr Expr)
;; type Op1 = ...
;;          | 'vector?
;; type Op2 = ...
;;          | 'make-vector | 'vector-ref
;; type Op3 = 'vector-set!
}

@section{Meaning of @this-lang programs, implicitly}

The @this-lang interpreter is essentially the same as for Hustle,
although with the addition of ternary primitives, plus an extension of
the @racket[interp-prims] module:

@codeblock-include["hoax/interp-prims.rkt"]

Vectors are easy to model in the interpreter because we can rely on
vectors in the meta-level of Racket.

This of course doesn't illuminate much about these operations.  We
could, as we did for Hustle, develop an interpeter with an explicit
account of memory.  Instead, let's just jump into the details of the
compiler.

@section{Representing @this-lang values}

Now that were are comfortable with heap-allocated data-structures like
boxes and pairs, handling vectors is not too difficult.  Vectors are
similarly heap allocated.  This will require a new kind of pointer value:

@verbatim{
- values
  + pointers (non-zero in last 3 bits)
    * boxes
    * pairs
    * vectors
  + immediates (zero in last three bits)
    * integers
    * characters
    * booleans
    * ...
}

We will follow exactly the same scheme we followed for box and pairs:
a vector will be uniquely tagged in the lowest three bits and the
remaining bits will indicate an address in memory which can be
obtained by zeroing out the tag bits.

The memory that is pointed to by a vector pointer will contain the
size of the vector followed by that many words of memory, one for each
element of the vector.

So for example the following creates a vector of size 3 containing the
values @racket[1], @racket[#t], @racket[#\c]:

@#reader scribble/comment-reader
(racketblock
(seq (Mov (Offset 'rbx 0) 3)       ; write vector length 3
     (Mov 'rax (value->bits 1))
     (Mov (Offset 'rbx 8) 'rax)    ; write 1 in vector slot 0
     (Mov 'rax (value->bits #t))
     (Mov (Offset 'rbx 16) 'rax)   ; write #t in vector slot 1
     (Mov 'rax (value->bits #\c))
     (Mov (Offset 'rbx 24) 'rax)   ; write #\c in vector slot 2
     (Mov 'rax 'rbx)
     (Or 'rax type-vect)           ; tag pointer as a vector
     (Add 'rbx 32))                ; advance rbx four words
)

Notice that the value written at offset @racket[0] is @racket[3], not
@racket[(value->bits 3)].  This is because this slot of memory in a
vector can only hold an integer, not an arbitrary value, so there's no
need to encode the type into the value---it's position tells us it's
an integer.

Now let's consider referencing elements of a vector.  If @racket['rax]
holds a vector value, we can reference an element of the vector by
untagging the value and fetching from an appropriate offset.  Suppose
we want to fetch the 2nd element (i.e. index @racket[1]) of a vector
in @racket['rax]:

@#reader scribble/comment-reader
(racketblock
(seq (Xor 'rax type-vect)         ; erase the vector tag
     (Mov 'rax (Offset 'rax 16))) ; load index 1 into rax
)

Notice that the offset here is @racket[16] because the first word is
the length, so the second word is the first element, and the third
word (offset 16) is the element we want.

This code assumes the vector has a length of at least two.  In
general, the vector operations must check that the given index is
valid for the vector.  This is accomplished by checking against the
length stored in the first word of the vector's memory.  Using
@racket['r9] as a scratch register, we could insert a check as
follows:

@#reader scribble/comment-reader
(racketblock
(seq (Xor 'rax type-vect)         ; erase the vector tag
     (Mov 'r9 (Offset 'rax 0))    ; load length into r9
     (Cmp 'r9 2)                  ; see if len < 2,
     (Jl 'raise_error)            ; raise error if so, otherwise
     (Mov 'rax (Offset 'rax 16))) ; load index 1 into rax
)

Suppose @racket['rax] holds a vector value and we want to update the
2nd element (i.e. index @racket[1]) to be @racket[#f].  Following the
outline above, we can erase the vector tag, check that the index is
valid, and then, rather than loading the element from memory, we can
write the new element at the appropriate offset:

@#reader scribble/comment-reader
(racketblock
(seq (Xor 'rax type-vect)         ; erase the vector tag
     (Mov 'r9 (Offset 'rax 0))    ; load length into r9
     (Cmp 'r9 2)                  ; see if len < 2,
     (Jl 'raise_error)            ; raise error if so, otherwise
     (Mov (Offset 'rax 16)
          (value->bits #f))) ;    ; write #f into index 1
)


@section{A Compiler for @this-lang}

Most of the work for the @this-lang compiler is done in the
compilation of the new operations:

@codeblock-include["hoax/compile-ops.rkt"]

We can now confirm that the compiler generates code similar to what we
wrote by hand above:

@ex[
(define (show e c)
  (compile-e (parse e) c))

(show '(make-vector 3 #t) '())
]


@section{A Run-Time for @this-lang}

First, we extend the value interface to include vectors:

@filebox-include[fancy-c "hoax/values.h"]

The implementation of @tt{val_typeof} is extended to handle
another pointer type:

@filebox-include[fancy-c "hoax/values.c"]

Printing is updated to handle vectors:

@filebox-include[fancy-c "hoax/print.c"]
