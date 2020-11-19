#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  "hustle/semantics.rkt"
	  "utils.rkt"
	  "ev.rkt"
  	  "../fancyverb.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "hustle" f))))))
	   '() #;'("interp.rkt" "ast.rkt" "parse.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt"))

@title[#:tag "Hustle"]{Hustle: heaps and lists}


@emph{A little and a little, collected together, become a great deal;
the heap in the barn consists of single grains, and drop and drop
makes an inundation.}

@table-of-contents[]

@section{Inductive data}

So far all of the data we have considered can fit in a single machine
word (64-bits).  Well, integers can't, but we truncated them and only
consider, by fiat, those integers that fit into a register.

In the @bold{Hustle} language, we will add two @bold{inductively
defined data types}, boxes and pairs, which will require us to relax
this restriction.

Boxes are like unary pairs, they simply hold a value, which can be
projected out.  Pairs hold two values which each can be projected out.

The new operations include constructors @racket[(box _e)] and
@racket[(cons _e0 _e1)] and projections @racket[(unbox _e)],
@racket[(car _e)], and @racket[(cdr _e)].

@margin-note{Usually boxes are @emph{mutable} data structures, like
OCaml's @tt{ref} type, but we will examine this aspect later.  For now,
we treat boxes as immutable data structures.}

These features will operate like their Racket counterparts:
@ex[
(unbox (box 7))
(car (cons 3 4))
(cdr (cons 3 4))
]


We use the following grammar for Hustle:

@centered[(render-language H)]

We can model this as an AST data type:

@codeblock-include["hustle/ast.rkt"]

@section{Meaning of Hustle programs}

The meaning of Hustle programs is just a slight update to Grift
programs, namely we add a few new primitives.

The update to the semantics is just an extension of the semantics of
primitives:

@(judgment-form-cases #f)

@;centered[(render-judgment-form 𝑯-𝒆𝒏𝒗)]

@centered[(render-metafunction 𝑯-𝒑𝒓𝒊𝒎 #:contract? #t)]

The interpreter similarly has an update to the @racket[interp-prim]
function.  On the relevant bits of
@link["hustle/interp.rkt"]{@tt{interp.rkt}} are shown:

@#reader scribble/comment-reader
(racketblock
;; Any -> Boolean
(define (prim? x)
  (and (symbol? x)
       (memq x '(add1 sub1 + - zero?
                      ;; New
                      box unbox cons car cdr))))

;; Prim [Listof Answer] -> Answer
(define (interp-prim p as)
  (match (cons p as)
    [(list 'add1 (? integer? i0)) (+ i0 1)]
    [(list 'sub1 (? integer? i0)) (- i0 1)]
    [(list 'zero? (? integer? i0)) (zero? i0)]
    [(list '+ (? integer? i0) (? integer? i1)) (+ i0 i1)]
    [(list '- (? integer? i0) (? integer? i1)) (- i0 i1)]
    ;; New for Hustle
    [(list 'box v0) (box v0)]
    [(list 'unbox (? box? v0)) (unbox v0)]    
    [(list 'cons v0 v1) (cons v0 v1)]
    [(list 'car (cons v0 v1)) v0]
    [(list 'cdr (cons v0 v1)) v1]
    [_ 'err]))
)

Inductively defined data is easy to model in the semantics and
interpreter because we can rely on inductively defined data at the
meta-level in math or Racket, respectively.

The real trickiness comes when we want to model such data in an
impoverished setting that doesn't have such things, which of course is
the case in assembly.

The problem is that a value such as @racket[(box _v)] has a value
inside it.  Pairs are even worse: @racket[(cons _v0 _v1)] has
@emph{two} values inside it.  If each value is represented with 64
bits, it would seem a pair takes @emph{at a minimum} 128-bits to
represent (plus we need some bits to indicate this value is a pair).
What's worse, those @racket[_v0] and @racket[_v1] may themselves be
pairs or boxes.  The great power of inductive data is that an
arbitrarily large piece of data can be constructed.  But it would seem
impossible to represent each piece of data with a fixed set of bits.

The solution is to @bold{allocate} such data in memory, which can in
principle be arbitrarily large, and use a @bold{pointer} to refer to
the place in memory that contains the data.

@;{ Really deserves a "bit" level interpreter to bring this idea across. }


@;codeblock-include["hustle/interp.rkt"]

@section{A Compiler for Hustle}

The first thing do is make another distinction in the kind of values
in our language.  Up until now, each value could be represented in a
register.  We now call such values @bold{immediate} values.

We introduce a new category of values which are @bold{pointer} values.
We will (for now) have two types of pointer values: boxes and pairs.

So we now have a kind of hierarchy of values:

@verbatim{
- values
  + pointers (non-zero in last 3 bits)
    * boxes
    * pairs
  + immediates (zero in last three bits)
    * integers
    * characters
    * booleans
    * ...
}

We will represent this hierarchy by shifting all the immediates over 3
bits and using the lower 3 bits to tag things as either being
immediate (tagged @code[#:lang "racket"]{#b000}) or a box or pair.
To recover an immediate value, we just shift back to the right 3 bits.

The pointer types will be tagged in the lowest three bits.  A box
value is tagged @code[#:lang "racket"]{#b001} and a pair is tagged
@code[#:lang "racket"]{#b010}.  The remaining 61 bits will hold a
pointer, i.e. an integer denoting an address in memory.

The idea is that the values contained within a box or pair will be
located in memory at this address.  If the pointer is a box pointer,
reading 64 bits from that location in memory will produce the boxed
value.  If the pointer is a pair pointer, reading the first 64 bits
from that location in memory will produce one of the value in the pair
and reading the next 64 bits will produce the other.  In other words,
constructors allocate and initialize memory.  Projections dereference
memory.

The representation of pointers will follow a slightly different scheme
than that used for immediates.  Let's first talk a bit about memory
and addresses.

A memory location is represented (of course, it's all we have!) as a
number.  The number refers to some address in memory.  On an x86
machine, memory is @bold{byte-addressable}, which means each address
refers to a 1-byte (8-bit) segment of memory.  If you have an address
and you add 1 to it, you are refering to memory starting 8-bits from the
original address.

We will make a simplifying assumption and always store things in
memory in multiples of 64-bit chunks.  So to go from one memory
address to the next @bold{word} of memory, we need to add 8 (1-byte
times 8 = 64 bits) to the address.

What is 8 in binary?  @code[#:lang "racket"]{#b1000}

What's nice about this is that if we start from a memory location that
is ``word-aligned,'' i.e. it ends in @code[#:lang "racket"]{#b000},
then every 64-bit index also ends in @code[#:lang "racket"]{#b000}.

What this means is that @emph{every} address we'd like to represent
has @code[#:lang "racket"]{#b000} in its least signficant bits.  We
can therefore freely uses these three bits to tag the type of the
pointer @emph{without needing to shift the address around}.  If we
have a box pointer, we can simply zero out the box type tag to obtain
the address of the boxes content.  Likewise with pairs.


We use a register, @racket['rdi], to hold the address of the next free
memory location in memory.  To allocate memory, we simply increment
the content of @racket['rdi] by a multiple of 8.  To initialize the
memory, we just write into the memory at that location.  To contruct a
pair or box value, we just tag the unused bits of the address.

So for example the following creates a box containing the value 7:

@#reader scribble/comment-reader
(racketblock
(seq (Mov 'rax (arithmetic-shift 7 imm-shift))  
     (Mov (Offset 'rdi 0) 'rax) ; write '7' into address held by rdi
     (Mov 'rax 'rdi)            ; copy pointer into return register
     (Or 'rax type-box)         ; tag pointer as a box
     (Add 'rdi 8))              ; advance rdi one word
)

If @racket['rax] holds a box value, we can ``unbox'' it by erasing the
box tag, leaving just the address of the box contents, then
dereferencing the memory:

@#reader scribble/comment-reader
(racketblock
(seq (Xor 'rax type-box)         ; erase the box tag
     (Mov 'rax (Offset 'rax 0))) ; load memory into rax
)

Pairs are similar.  Suppose we want to make @racket[(cons 3 4)]:

@#reader scribble/comment-reader
(racketblock
(seq (Mov 'rax (arithmetic-shift 3 imm-shift))
     (Mov (Offset 'rdi 0) 'rax) ; write '3' into address held by rdi
     (Mov 'rax (arithmetic-shift 4 imm-shift))
     (Mov (Offset 'rdi 1) 'rax) ; write '4' into word after address held by rdi
     (Mov 'rax rdi)             ; copy pointer into return register
     (Or 'rax type-pair)        ; tag pointer as a pair
     (Add 'rdi 16))             ; advance rdi 2 words
)

If @racket['rax] holds a pair value, we can project out the elements
by erasing the pair tag, leaving just the address of the pair contents,
then dereferencing either the first or second word of memory:

@#reader scribble/comment-reader
(racketblock
(seq (Xor 'rax type-pair)         ; erase the pair tag
     (Mov 'rax (Offset 'rax 0))   ; load car into rax
     (Mov 'rax (Offset 'rax 1)))  ; or... load cdr into rax
)

From here, writing the compiler for @racket[box], @racket[unbox],
@racket[cons], @racket[car], and @racket[cdr] is just a matter of
putting together pieces we've already seen such as evaluating multiple
subexpressions and type tag checking before doing projections.



The complete compiler is given below.

@codeblock-include["hustle/compile.rkt"]

@section{A Run-Time for Hustle}

The run-time system for Hustle is more involved for two main reasons:

The first is that the compiler relies on a pointer to free memory
residing in @racket['rdi].  The run-time system will be responsible
for allocating this memory and initializing the @racket['rdi]
register.  To allocate memory, it uses @tt{malloc}.  It passes the
pointer returned by @tt{malloc} to the @tt{entry} function.  The
protocol for calling functions in C says that the first argument will
be passed in the @racket['rdi] register.  Since @tt{malloc} produces
16-byte aligned addresses on 64-bit machines, @racket['rdi] is
initialized with an address that ends in @code[#:lang
"racket"]{#b000}, satisfying our assumption about addresses.

The second complication comes from printing.  Now that values include
inductively defined data, the printer must recursively traverse these
values to print them.

The complete run-time system is below.

@filebox-include[fancy-c "hustle/main.c"]
