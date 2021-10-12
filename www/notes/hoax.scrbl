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

@title[#:tag this-lang]{@|this-lang|: vectors and strings}


@emph{Stupidity, outrage, vanity, cruelty, iniquity, bad faith,
falsehood - we fail to see the whole array when it is facing in the
same direction as we.}

@table-of-contents[]

@section{Array data}

In the @bold{@this-lang} language, we will add two @bold{array
data types}: vectors and strings.

Vectors are fixed-length arrays of values with constant-time access
and update of the vector slots, which are numbered from @racket[0] to
one less than the number of slots in the vector.

Strings are fixed-length arrays of characters with constant-time
access and update of the character slots, which are numbered from
@racket[0] to one less than the number of slots in the string.

The new vector operations include the constructor @racket[(make-vector
_e1 _e2)], predicate @racket[(vector? _e0)], accessor
@racket[(vector-ref _e0 _e1)] and mutator @racket[(vector-set! _e0 _e1
_e2)].

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

The new string operations include the constructor @racket[(make-string
_e1 _e2)], predicate @racket[(string? _e0)], and accessor
@racket[(string-ref _e0 _e1)].  We will also add support for string
literals.

These features will operate like their Racket counterparts:
@ex[
(make-string 3 #\t)
(string? (make-string 3 #\t))
(string-ref "abc" 0)
(string-ref "abc" 2)
]



We can model this syntax as an AST data type:

@filebox-include-fake[codeblock "hoax/ast.rkt"]{
#lang racket
;; type Expr = ...
;;           | (Prim3 Op3 Expr Expr Expr)
;; type Op1 = ...
;;          | 'vector? | 'string?
;; type Op2 = ...
;;          | 'make-vector | 'vector-ref
;;          | 'make-string | 'string-ref
;; type Op3 = 'vector-set!
}

@section{Meaning of @this-lang programs, implicitly}

We extend our definition of values, representing vectors with vectors
and strings with strings (what a surprise!):

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
;; | (string Character ...)
)

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
    * strings
  + immediates (zero in last three bits)
    * integers
    * characters
    * booleans
    * ...
}

We will follow exactly the same scheme we followed for box and pairs:
vectors and strings will be uniquely tagged in the lowest three bits
and the remaining bits will indicate an address in memory which can be
obtained by zeroing out the tag bits.

@section{Representing and operating on vectors}

The memory that is pointed to by a vector pointer will contain the
size of the vector followed by that many words of memory, one for each
element of the vector. (Strings will be similar, with a slight twist,
which we'll examine later.)

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

One final issue for vectors is what to do about the empty vector.

An empty vector has length zero and there are no elements contained
within it.  We could reprent empty vectors the same as non-empty
vectors, although this would mean allocating a word of memory to hold
the length @racket[0] and pointing to it.  This design would also have
the drawback that there could many @emph{different} empty vectors.

Another approach is to avoid allocating memory and have a single
representation for the empty vector.  One way to achieve this to
represent the address of the empty vector as the null pointer
(i.e. @racket[0]) and therefore the empty vector value is represented
by the vector type tag.  Some code, such as the code to print vectors,
will need to have a special case for the empty vector to avoid a null
dereference when trying to load the length of the vector.  Similarly,
there will be a special case in the implementation of
@racket[make-vector] to produce the empty vector value when given a
size of zero.  This avoids allocating memory for the empty vector and
has the nice benefit that there is a unique representation of the
empty vector.

@section{Representing and operating on strings}

Strings will be very much like vectors---after all, they are just
another kind of array value.  The key difference is that strings
are arrays not of arbitrary values, but of characters.

While could use a vector to represent a string, with a unique pointer
tag, this would waste memory: every character would be allocated
64-bits of memory.  Since we use unicode codepoints to represent
characters and because strings are @bold{homogenous} we need at most
21-bits to represent each character of a string.

There are many different representations for strings of Unicode
characters, but one of the simplest is that of UTF-32.  It is a
fixed-width encoding that uses 32-bits for each character.  This is
still wasteful, but has the benefit of supporting @racket[string-ref]
in constant time.  Had we not needed to implement @racket[string-ref]
with this guarantee, other less wasteful encodings such as UTF-8 could
be used.  We'll use UTF-32 as a nice balance of simplicity and economy
of memory usage.

So the basic idea will be that a string will be represented by a
distinct tag in the lower three bits of an 8-byte aligned address.
The pointer points to memory where, like a vector, the first word
holds the length of the string, followed by an array of 32-bit slots,
each holding a character codepoint.

There is a wrinkle: an odd length string would seemingly occupy a
segment of memory that does not fall on an 8-byte boundary.  For
example, a string of length 1 would occupy 64+32=96-bits=12-bytes of
memory.  This would violate our assumption that the next free memory
address ends in @code[#:lang "racket"]{#b000}.

The solution is simple: allocate 32-bits more when the length is odd.
This sacrafices a small amount of memory in order to preserve the
invariant that allows our low-order tagging of pointers.

Another complication is that we will now want to read and write
32-bits of memory.  Until now, we've always operated on memory in
units of 64-bits.  We could ``fake it'' by reading and writing 64-bits
at a time, carefully making sure to ignore or preserve half of the
bits, however this makes the code a mess and is inefficient.

The better solution is to introduce a 32-bit register: @racket['eax].
The @racket['eax] register is not actually a new register, but rather
is a name for the lower 32-bits of @racket['rax] (so be careful:
modifying one will change the other---they are the same register!).
Whenever @racket['eax] is used in a memory read or write, the CPU will
read or write 32-bits instead of 64.

So, suppose we want to create the string @racket["abc"]:

@#reader scribble/comment-reader
(racketblock
(seq (Mov (Offset 'rbx 0) 3)       ; write string length 3
     (Mov 'eax (char->integer #\a))
     (Mov (Offset 'rbx 8) 'eax)    ; write #\a in string slot 0
     (Mov 'eax (char->integer #\b))
     (Mov (Offset 'rbx 12) 'eax)   ; write #\b in string slot 1
     (Mov 'eax (char->integer #\c))
     (Mov (Offset 'ebx 16) 'rax)   ; write #\c in string slot 3
     (Mov 'rax 'rbx)
     (Or 'rax type-str)            ; tag pointer as a string
     (Add 'rbx 24))                ; advance rbx three words(!)
)

This looks a lot like the creation of a vector, however note that we
@itemlist[
@item{use @racket['eax] to write 32-bits of memory,}
@item{advance the offset by 4-bytes (32-bits) on each subsequent character,}
@item{write @racket[(char->integer #\a)] instead of @racket[(value->bits #\a)] into memory,}
@item{increment @racket['rbx] by 24, even though we've only written 20 bits.}
]

Now let’s consider referencing elements of a string.  Suppose
@racket['rax] holds a string value, we can reference an element of the
string by untagging the value and fetching from an appropriate offset.
This is just like referencing an element of a vector, except:
@itemlist[
@item{the offset will be computed differently,}
@item{only 32-bits should be loaded from memory, and}
@item{the codepoint needs to be converted into a character.}]

Suppose we want to fetch the 2nd element (i.e. index @racket[1]) of a
string in @racket['rax]:

@#reader scribble/comment-reader
(racketblock
(seq (Xor 'rax type-str)          ; erase the string tag
     (Mov 'eax (Offset 'rax 12))  ; load index 1 into eax
     (Sal 'rax char-shift)
     (Or 'rax char-type))         ; convert codepoint to character
)

Note the use of offset @racket[12] here: 8-bytes to skip past the
length plus 4 bytes to skip past the first character.  The
@racket['eax] register is used to load 32-bits of memory, then the
value is converted to a character by shifting and tagging.

Just as we did with vectors, we want the compiler to emit code that
checks indices are inbound for the string.


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
(show '(vector-ref x 1) '(x))
(show '"abc" '())
(show '(string-ref x 1) '(x))
]


@section{A Run-Time for @this-lang}

First, we extend the value interface to include vectors:

@filebox-include[fancy-c "hoax/values.h"]

The implementation of @tt{val_typeof} is extended to handle
another pointer type:

@filebox-include[fancy-c "hoax/values.c"]

Printing is updated to handle vectors and strings.  Note that printing
of strings seems complicated by this code is actually auto-generated
from the Unicode specification.

@filebox-include[fancy-c "hoax/print.c"]
