#lang scribble/manual

@(require (for-label (except-in racket ... compile) a86))
@(require redex/pict
          racket/runtime-path
          scribble/examples
          racket/format
          "../fancyverb.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  "../../langs/dupe/semantics.rkt"
          "../../langs/dupe/types.rkt"
	  "../utils.rkt")



@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "dupe" f))))))
	   '("interp.rkt" "compile.rkt" "ast.rkt" "parse.rkt" "random.rkt" "types.rkt"))


@title[#:tag "Dupe"]{Dupe: a duplicity of types}

@emph{There are 10 types of values...}

@table-of-contents[]

@section{Integers and Booleans}

Up until now we have worked to maintain a single type of values:
integers.  This led to some awkwardness in the design of conditionals.
Let us lift this restriction by considering a multiplicity of types.
To start, we will consider two: integers and booleans.

We'll call it @bold{Dupe}.

We will use the following syntax, which relaxes the syntax of Con to
make @racket['(zero? _e)] its own expression form and conditionals can
now have arbitrary expressions in test position: @racket[(if _e0 _e1
_e2)] instead of just @racket[(if (zero? _e0) _e1 _e2)].  We also add
syntax for boolean literals.

Together this leads to the following grammar for concrete Dupe. 

@centered{@render-language[D-concrete]}

And abstract Dupe:

@centered{@render-language[D]}


One thing to take note of is the new nonterminal @math{v}
which ranges over @bold{values}, which are integers and
booleans.

Abstract syntax is modelled with the following datatype definition:

@codeblock-include["dupe/ast.rkt"]

The s-expression parser is defined as follows:

@codeblock-include["dupe/parse.rkt"]

@section{Meaning of Dupe programs}

To consider he meaning of Dupe programs, we must revisit the meaning
of conditions.  Previously we branched on whether a subexpression
evaluated to 0.  We will now consider whether the subexpression
evaluates to @racket[#f].

Let's consider some examples:

@itemlist[

@item{@racket[(if #f 1 2)] means @racket[2] since the test expression means @racket[#f].}
@item{@racket[(if #t 1 2)] means @racket[1] since the test expression means @racket[#t], which is not @racket[#f].}

]

Just as in Racket (and Scheme, generally), we will treat any
non-@racket[#f] value as ``true.'' So:

@itemlist[

@item{@racket[(if 0 1 2)] means @racket[1] since the test expression means @racket[0], which is not @racket[#f].}
@item{@racket[(if 7 1 2)] means @racket[1] since the test expression means @racket[7], which is not @racket[#f].}

]

The new @racket[(zero? _e)] expression form evaluates to a boolean:
@racket[#t] when @racket[_e] evaluates to @racket[0] and @racket[#f]
to an integer other than @racket[0].

The @racket[#t] and @racket[#f] literals mean themselves, just like
integer literals.


Once a multiplicity of types are introduced, we are forced to come to
grips with @bold{type mismatches}.  For example, what should
@racket[(add1 #f)] mean?

Languages adopt several approaches:

@itemlist[

@item{Prohibit such programs statically with a type system (e.g. OCaml, Java)}
@item{Coerce values to different types (e.g. JavaScript)}
@item{Signal a run-time error (e.g. Racket)}
@item{Leave the behavior unspecified (e.g. Scheme, C)}

]

We are going to start by taking the last approach.  Later we can
reconsider the design, but for now this is a simple approach.


The semantics is still a binary relation between expressions and their
meaning, however the type of the relation is changed to reflect the
values of Dupe, which may either be integers or booleans:


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

@(show-judgment 𝑫 0 3)
@(show-judgment 𝑫 3 5)
@(show-judgment 𝑫 5 7)

This relies on two helper judgments:

@(show-judgment is-true 0 2)
@(show-judgment is-false 0 1)

Notice that here we are following the semantics of Racket, which says
that anything that is not @racket[#f] counts as @emph{true} in a
conditional.

This semantics has also been refactored slightly so that
there is a single rule for the @racket[Prim1] case. The new
rule essentially defers the work to a new metafunction,
@math{𝑫-𝒑𝒓𝒊𝒎}:

@(centered
  (with-compound-rewriters (['+ (rewrite '+)]
                            ['- (rewrite '–)]
                            ['= (rewrite '=)]
                            ['!= (rewrite '≠)])
    (render-metafunction 𝑫-𝒑𝒓𝒊𝒎 #:contract? #t)))

Returning to the issue of type mismatches, what does the
semantics say about @racket[(Prim1 'add1 (Bool #f))]?

What it says is: nothing.  These programs are simply not in the
semantic relation for this language. There's only one rule for giving
meaning to an @racket[(Prim1 'add1 _e0)] expression and it's premise is that
@racket[_e] means some @emph{integer} @racket[_i0].  But
@math{(@racket[(Bool #f)], i) ∉ 𝑫} for any @math{i}.  So there's no value
@math{v} such that @math{(@racket[(Prim1 'add1 (Bool #f))], v) ∈ 𝑫}.  This
expression is @bold{undefined} according to the semantics.


The interpreter follows the rules of the semantics closely and is
straightforward:

@codeblock-include["dupe/interp.rkt"]

And the interpretation of primitives closely matches @math{𝑫-𝒑𝒓𝒊𝒎}:

@codeblock-include["dupe/interp-prim.rkt"]

We can confirm the interpreter computes the right result for the
examples given earlier:

@ex[
(interp (Bool #t))
(interp (Bool #f))
(interp (If (Bool #f) (Int 1) (Int 2)))
(interp (If (Bool #t) (Int 1) (Int 2)))
(interp (If (Int 0) (Int 1) (Int 2)))
(interp (If (Int 7) (Int 1) (Int 2)))
(interp (If (Prim1 'zero? (Int 7)) (Int 1) (Int 2)))
]

Correctness follows the same pattern as before, although it is worth
keeping in mind the ``hypothetical'' form of the statement: @emph{if}
the expression has some meaning, then the interpreter must produce it.
In cases where the semantics of the expression is undefined, the
interpreter can do whatever it pleases; there is no specification.


@bold{Interpreter Correctness}: @emph{For all Dupe expressions
@racket[e] and values @racket[v], if (@racket[e],@racket[v]) in
@render-term[D 𝑫], then @racket[(interp e)] equals
@racket[v].}

Consider what happens with @racket[interp] on undefined programs such
as @racket[(add1 #f)]: the interpretation of this expression is just
the application of the Racket @racket[add1] function to @racket[#f],
which results in the @racket[interp] program crashing and Racket
signalling an error:

@ex[
(eval:error (interp (Prim1 'add1 (Bool #f))))
]

This isn't a concern for correctness, because the interpreter is free
to crash (or do anything else) on undefined programs; it's not in
disagreement with the semantics, because there is no semantics.

From a pragmatic point of view, this is a concern because it
complicates testing.  If the interpreter is correct and every
expression has a meaning (as in all of our previous languages), it
follows that the interpreter can't crash on any @tt{Expr} input.  That
makes testing easy: think of an expression, run the interpreter to
compute its meaning.  But now the interpreter may break if the
expression is undefined.  We can only safely run the interpreter on
expressions that have a meaning.

We'll return to this point in the design of later langauges.

@section{Ex uno plures: Out of One, Many}

Before getting in to the compiler, let's first address the issue of
representation of values in the setting of x86.

So far we've had a single type of value: integers.  Luckily, x86 has a
convenient datatype for representing integers: it has 64-bit signed
integers.  (There is of course the problem that this representation is
@bold{inadequate}; there are many (Con) integers we cannot represent
as a 64-bit integer.  But we put off worrying about this until later.)

The problem now is how to represent integers @emph{and} booleans,
which should be @bold{disjoint} sets of values.  Representing these
things in the interpreter as Racket values was easy: we used booleans
and integers.  Representing this things in x86 will be more
complicated.  x86 doesn't have a notion of ``boolean'' per se and it
doesn't have a notion of ``disjoint'' datatypes.  There is only one
data type and that is: bits.

We chose 64-bit integers to represent Con integers, because that's the
kind of value that can be stored in a register or used as an argument
to a instruction.  It's the kind of thing that can be returned to the
C run-time.  It's (more or less) the only kind of thing we have to
work with.  So had we started with booleans instead of integers, we
still would have represented values as a sequence of bits
@emph{because that's all there is}.  Now that we have booleans
@emph{and} integers, we will have to represent both as bits (64-bit
integers).  The bits will have to encode both the value and the
@emph{type} of value.

@margin-note{As discussed in the lecture video, there are many possible ways of
representing multiple types, this is just how we've decided to do it.}

Here is the idea of how this could be done: We have two kinds of data:
integers and booleans, so we could use one bit to indicate whether a
value is a boolean or an integer.  The remaining 63 bits can be used
to represent the value itself, either true, false, or some integer.

Let's use the least significant bit to indicate the type and
let's use @binary[type-int] for integer and
@binary[type-bool] for boolean. These are arbitrary choices
(more or less).

The number @racket[1] would be represented as
@binary[(value->bits 1)], which is the bits for the number
(i.e. @binary[1]), followed by the integer tag
(@binary[type-int]). Note that the representation of a Dupe
number is no longer the number itself: the Dupe value
@racket[1] is represented by the number @racket[2]
(@binary[2]). The Dupe value @racket[#t]
is represented by the number @racket[#,val-true]
(@binary[val-true 2]); the Dupe value @racket[#f]
is represented by the number @racket[#,val-false]
(@binary[val-false 2]).

One nice thing about our choice of encoding: @racket[0] is represented
as @racket[0] (@binary[0 2]).

If you wanted to determine if a 64-bit integer represented
an integer or a boolean, you simply need to inquire about
the value of the least significant bit. At a high-level,
this just corresponds to asking if the number is even or
odd. Odd numbers end in the bit (@binary[1]), so they
reprepresent booleans. Even numbers represent integers. Here
are some functions to check our understanding of the
encoding:

@codeblock-include["dupe/types.rkt"]

@#reader scribble/comment-reader
(ex 

(bits->value #b000)
(bits->value #b001)
(bits->value #b010)
(bits->value #b011)
(bits->value #b100)
(eval:error (bits->value #b101))
(bits->value #b110)
(eval:error (bits->value #b111))

)

Notice that not all bits represent a value; name any odd number that's
neither 1 (@racket[#f]) or 3 (@racket[#t]).

We can also write the inverse:

@#reader scribble/comment-reader
(ex

(value->bits #t)
(value->bits #f)
(value->bits 0)
(value->bits 1)
(value->bits 2)
(value->bits 3)


(bits->value (value->bits #t))
(bits->value (value->bits #f))
(bits->value (value->bits 0))
(bits->value (value->bits 1))
(bits->value (value->bits 2))
(bits->value (value->bits 3))

)

The interpreter operates at the level of @tt{Value}s.  The compiler
will have to work at the level of @tt{Bits}.  Of course, we could,
as an intermediate step, define an interpreter that works on bits,
which may help us think about how to implement the compiler.

We want a function
@#reader scribble/comment-reader
(racketblock
;; interp-bits : Expr -> Bits
)
such that
@#reader scribble/comment-reader
(racketblock
;; ∀ e : Expr . (interp-bits e) = (value->bits (interp e))
)

Let's design @racket[interp-bits] by derivation.  This is a common
functional programming technique whereby we start from a specification
program and through a series of algebraic transformations, arrive an
correct implementation.

First, let's state the specification of @racket[interp-bits] as a
function that ``cheats,'' it uses @racket[interp] to carry out
evaluation and then finally converts to bits.

@#reader scribble/comment-reader
(ex
;; Expr -> Bits
(define (interp-bits e)
  (value->bits (interp e)))
)

It's clearly correct with respect to the spec for @racket[interp-bits]
that we started with because @emph{the code is just the specification
itself}.

We can even do some property-based random testing (which obviously
succeeds):

@ex[#:no-prompt
(define (interp-bits-correct e)
  (with-handlers ([exn:fail? (λ (x) 'ok)])
    (interp e)
    (check-equal? (interp-bits e)
                  (value->bits (interp e)))))

(define es
  (for/list ([i 100])
    (random-expr)))

(for-each interp-bits-correct es)
]

The one wrinkle is we really only need the spec to hold when
@racket[_e] is defined according to the semantics.  Since we know
@racket[interp] crashes (by raising an exception) whenever @racket[_e]
is undefined, we use an exception handler to avoid testing when
@racket[_e] is undefined.

Now let us inline the defintion of @racket[interp], i.e. let's replace
the use of @racket[interp] with it's definition:

@#reader scribble/comment-reader
(ex #:no-prompt
;; Expr -> Bits
(define (interp-bits e)
  (value->bits
   (match e
     [(Int i) i]
     [(Bool b) b]
     [(Prim1 p e)
      (interp-prim1 p (interp e))]
     [(If e1 e2 e3)
      (if (interp e1)
          (interp e2)
          (interp e3))])))
)

It's still correct:
@ex[
(for-each interp-bits-correct es)]

Now let's ``push'' the @racket[value->bits] inward by using the following equation:
@racketblock[
(_f (match _m [_p _e] ...)) = (match _m [_p (_f _e)] ...)
]

So we get:

@#reader scribble/comment-reader
(ex #:no-prompt
;; Expr -> Bits
(define (interp-bits e)
  (match e
    [(Int i) (value->bits i)]
    [(Bool b) (value->bits b)]
    [(Prim1 p e)
     (value->bits
      (interp-prim1 p (interp e)))]
    [(If e1 e2 e3)
     (value->bits
      (if (interp e1)
          (interp e2)
          (interp e3)))])))
)


Still correct:
@ex[
(for-each interp-bits-correct es)]

In the first two cases, we know that @racket[i] and @racket[b] are
integers and booleans, respectively.  So we know @racket[(values->bits
i) = (* 2 i)] and @racket[(values->bits b) = (if b #,val-true #,val-false)].  We can
rewrite the code as:

@;{the #:escape identity thing is a cute solution to the
 problem of escaping within examples.}

@ex[#:escape identity #:no-prompt
@code:comment{Expr -> Bits}
 (define (interp-bits e)
   (match e
     [(Int i) (* 2 i)]
     [(Bool b) (if b (identity val-true) (identity val-false))]
     [(Prim1 'add1 e0)
      (value->bits (add1 (interp e0)))]
     [(Prim1 'sub1 e0)
      (value->bits (sub1 (interp e0)))]
     [(Prim1 'zero? e0)
      (value->bits (zero? (interp e0)))]
     [(If e0 e1 e2)
      (value->bits
       (if (interp e0)
           (interp e1)
           (interp e2)))]))
]

Still correct:
@ex[
(for-each interp-bits-correct es)]


We can rewrite the last case by the following equation:
@racketblock[
(_f (if _e0 _e1 _e2)) = (if _e0 (_f _e1) (_f _e2))
]

@ex[#:escape identity #:no-prompt
@code:comment{Expr -> Bits}
 (define (interp-bits e)
   (match e
     [(Int i) (* 2 i)]
     [(Bool b) (if b (identity val-true) (identity val-false))]
     [(Prim1 'add1 e0)
      (value->bits (add1 (interp e0)))]
     [(Prim1 'sub1 e0)
      (value->bits (sub1 (interp e0)))]
     [(Prim1 'zero? e0)
      (value->bits (zero? (interp e0)))]
     [(If e0 e1 e2)      
      (if (interp e0)
          (value->bits (interp e1))
          (value->bits (interp e2)))]))
]

Still correct:
@ex[
(for-each interp-bits-correct es)]


Let's now re-write by the following equations:

@racketblock[
(add1 _e) = (+ _e 1)
(sub1 _e) = (- _e 1)
(f (+ _e0 _e1)) = (+ (f _e0) (f _e1))
(f (- _e0 _e1)) = (- (f _e0) (f _e1))
]

to get:

@ex[#:escape identity #:no-prompt
@code:comment{Expr -> Bits}
 (define (interp-bits e)
   (match e
     [(Int i) (* 2 i)]
     [(Bool b) (if b (identity val-true) (identity val-false))]
     [(Prim1 'add1 e0)
      (+ (value->bits (interp e0)) (value->bits 1))]
     [(Prim1 'sub1 e0)
      (- (value->bits (interp e0)) (value->bits 1))]
     [(Prim1 'zero? e0)
      (value->bits (zero? (interp e0)))]
     [(If e0 e1 e2)      
      (if (interp e0)
          (value->bits (interp e1))
          (value->bits (interp e2)))]))
]

Still correct:
@ex[
(for-each interp-bits-correct es)]

By computation, @racket[(value->bits 1) = #,(value->bits 1)].

We can now rewrite by the equation of our specification:

@racketblock[
(value->bits (interp _e)) = (interp-bits _e)]


@ex[#:escape identity #:no-prompt
@code:comment{Expr -> Bits}
 (define (interp-bits e)
   (match e
     [(Int i) (* 2 i)]
     [(Bool b) (if b (identity val-true) (identity val-false))]
     [(Prim1 'add1 e0)
      (+ (interp-bits e0) (identity (value->bits 1)))]
     [(Prim1 'sub1 e0)
      (- (interp-bits e0) (identity (value->bits 1)))]
     [(Prim1 'zero? e0)
      (value->bits (zero? (interp e0)))]
     [(If e0 e1 e2)      
      (if (interp e0)
          (interp-bits e1)
          (interp-bits e2))]))
]

Still correct:
@ex[
(for-each interp-bits-correct es)]

We can rewrite by the equation
@racketblock[(zero? (interp _e0)) = (zero? (interp-bits _e0))]
and inline @racket[value->bits] specialized to a boolean argument:

@ex[#:escape identity #:no-prompt
@code:comment{Expr -> Bits}
 (define (interp-bits e)
   (match e
     [(Int i) (* 2 i)]
     [(Bool b) (if b (identity val-true) (identity val-false))]
     [(Prim1 'add1 e0)
      (+ (interp-bits e0) (identity (value->bits 1)))]
     [(Prim1 'sub1 e0)
      (- (interp-bits e0) (identity (value->bits 1)))]
     [(Prim1 'zero? e0)
      (match (zero? (interp-bits e0))
        [#t (identity val-true)]
        [#f (identity val-false)])]
     [(If e0 e1 e2)
      (if (interp e0)
          (interp-bits e1)
          (interp-bits e2))]))
 ]

Still correct:
@ex[
(for-each interp-bits-correct es)]

Finally, in the last case, all that matters in @racket[(if (interp e0)
...)] is whether @racket[(interp e0)] returns @racket[#f] or something
else.  So we can rewrite in terms of whether @racket[(interp-bits e0)]
produces the representation of @racket[#f] (@binary[val-false 2]):

@ex[#:escape identity #:no-prompt
@code:comment{Expr -> Bits}
 (define (interp-bits e)
   (match e
     [(Int i) (* 2 i)]
     [(Bool b) (if b (identity val-true) (identity val-false))]
     [(Prim1 'add1 e0)
      (+ (interp-bits e0) (identity (value->bits 1)))]
     [(Prim1 'sub1 e0)
      (- (interp-bits e0) (identity (value->bits 1)))]
     [(Prim1 'zero? e0)
      (match (zero? (interp-bits e0))
        [#t (identity val-true)]
        [#f (identity val-false)])]
     [(If e0 e1 e2)
      (if (= (interp-bits e0) (identity val-false))
          (interp-bits e2)
          (interp-bits e1))]))
 ]


Still correct:
@ex[
(for-each interp-bits-correct es)]


Note that whenever @racket[_bs] are bits representing an integer, then
@racket[(value->bits (add1 (bits->value _bs)))] is equal to @racket[(+
_bs #,(value->bits 1))], i.e. adding @binary[(value->bits 1) 2].  When @racket[_bs]
represents a boolean, then @racket[(value->bits (add1 (bits->value
_bs)))] would crash, while @racket[(+ _bs (value->bits 1))] doesn't, but this is an
undefined program, so changing the behavior is fine.

Looking back: starting from the spec, we've arrived at a definition of
@racket[interp-bits] that is completely self-contained: it doesn't use
@racket[interp] at all.  It also only uses the @tt{Bits}
representation and does no conversions to or from @tt{Value}s.

We can recover the original interpreter by wrapping the bit
interpreter in a final conversion:

@#reader scribble/comment-reader
(ex
;; Expr -> Value
(define (interp.v2 e)
  (bits->value (interp-bits e)))

(interp.v2 (Bool #t))
(interp.v2 (Bool #f))
(interp.v2 (parse '(if #f 1 2)))
(interp.v2 (parse '(if #t 1 2)))
(interp.v2 (parse '(if 0 1 2)))
(interp.v2 (parse '(if 7 1 2)))
(interp.v2 (parse '(if (zero? 7) 1 2)))
(eval:error (interp.v2 (parse '(add1 #f))))
(interp.v2 (parse '(add1 #t)))
)

Notice the last two examples.  What's going on?

The @racket[interp.v2] function is also a correct interpreter for
Dupe, and importantly, it sheds light on how to implement the compiler
since it uses the same representation of values.

@section{An Example of Dupe compilation}

The most significant change from Con to Dupe for the compiler is the
change in representation, but having sorted those issues out at the
level of @racket[interp-bits], it should be pretty easy to write the
compiler.

Let's consider some simple examples:

@itemlist[

@item{@racket[42]: this should compile just like integer literals
before, but needs to use the new representation, i.e. the compiler
should produce @racket[(Mov 'rax 84)], which is @racket[(* 42 2)].}

@item{@racket[#f]: this should produce @racket[(Mov 'rax #,val-false)].}

@item{@racket[#t]: this should produce @racket[(Mov 'rax #,val-true)].}

@item{@racket[(add1 _e)]: this should produce the instructions for
@racket[_e] followed by an instruction to add @racket[#,(value->bits 1)], which is
just how @racket[interp-bits] interprets an @racket[add1].}

@item{@racket[(sub1 _e)]: should work like @racket[(add1 _e)] but
subtracting @racket[#,(value->bits 1)].}

 @item{@racket[(zero? _e)]: this should produce the
  instructions for @racket[_e] followed by instructions that
  compare @racket['rax] to 0 and set @racket['rax] to
  @racket[#t] (i.e. @binary[val-true 2]) if true and
  @racket[#f] (i.e. @binary[val-false 2]) otherwise.}

@item{@racket[(if _e0 _e1 _e2)]: this should work much like before,
compiling each subexpression, generating some labels and the
appropriate comparison and conditional jump.  The only difference is
we now want to compare the result of executing @racket[_e0] with
@racket[#f] (i.e. @binary[val-false 2]) and jumping to the code for @racket[_e2] when
they are equal.}
]

@ex[
(compile-e (Int 42))
(compile-e (Bool #t))
(compile-e (Bool #f))
(compile-e (parse '(zero? 0)))
(compile-e (parse '(if #t 1 2)))
(compile-e (parse '(if #f 1 2)))
]


@section{A Compiler for Dupe}

Based on the examples, we can write the compiler:

@codeblock-include["dupe/compile.rkt"]

The compilation of primitives, including the new @racket[zero?]
primitive, can be accomplished with:

@codeblock-include["dupe/compile-ops.rkt"]

We can try out the compiler with the help of @racket[asm-interp],
but you'll notice the results are a bit surprising:

@ex[
(asm-interp (compile (Bool #t)))
(asm-interp (compile (Bool #f)))
(asm-interp (compile (parse '(zero? 0))))
(asm-interp (compile (parse '(zero? -7))))
(asm-interp (compile (parse '(if #t 1 2))))
(asm-interp (compile (parse '(if #f 1 2))))
(asm-interp (compile (parse '(if (zero? 0) (if (zero? 0) 8 9) 2))))
(asm-interp (compile (parse '(if (zero? (if (zero? 2) 1 0)) 4 5))))
]

The reason for this is @racket[asm-interp] doesn't do any
interpretation of the bits it gets back; it is simply
producing the integer that lives in @racket['rax] when the
assembly code finishes. This suggests adding a call to
@racket[bits->value] can be added to interpret the bits as
values:

@ex[
(define (interp-compile e)
  (bits->value (asm-interp (compile e))))

(interp-compile (Bool #t))
(interp-compile (Bool #f))
(interp-compile (parse '(zero? 0)))
(interp-compile (parse '(zero? -7)))
(interp-compile (parse '(if #t 1 2)))
(interp-compile (parse '(if #f 1 2)))
(interp-compile (parse '(if (zero? 0) (if (zero? 0) 8 9) 2)))
(interp-compile (parse '(if (zero? (if (zero? 2) 1 0)) 4 5)))
]



The one last peice of the puzzle is updating the run-time system to
incorporate the new representation.  The run-time system is
essentially playing the role of @racket[bits->value]: it determines
what is being represented and prints it appropriately.

For the run-time system, we define the bit representations in a header
file corresponding to the definitions given in @tt{types.rkt}:

@filebox-include[fancy-c "dupe/types.h"]

It uses an idiom of ``masking'' in order to examine on
particular bits of a value. So for example if we want to
know if the returned value is an integer, we do a
bitwise-and of the result and @tt{1}. This produces a single
bit: 0 for integer and 1 for boolean. In the case of an
integer, to recover the number being represented, we need to
divide by 2, which can be done efficiently with a
right-shift of 1 bit. Likewise with a boolean, if we shift
right by 1 bit there are two possible results:
@racket[#,val-false] for false and @racket[#,val-true] for
true.

We use the following interface for values in the runtime system:

@filebox-include[fancy-c "dupe/values.h"]
@filebox-include[fancy-c "dupe/values.c"]

The @tt{main} function remains largely the same although now we use
@tt{val_t} in place of @tt{int64_t}:

@filebox-include[fancy-c "dupe/main.c"]

And finally, @tt{print_result} is updated to do a case analysis on the
type of the result and print accordingly:

@filebox-include[fancy-c "dupe/print.c"]

@section{Correctness and testing}

We can randomly generate Dupe programs.  The problem is many randomly
generated programs will have type errors in them:

@ex[
(eval:alts (require "random.rkt") (void))
(random-expr)
(random-expr)
(random-expr)
(random-expr)
(random-expr)
(random-expr)
]

When interpreting programs with type errors, we get @emph{Racket}
errors, i.e. the Racket functions used in the implementation of the
interpreter will signal an error:
@ex[
(eval:error (interp (parse '(add1 #f))))
(eval:error (interp (parse '(if (zero? #t) 7 8))))
]

On the other hand, the compiler may produce bits that are illegal
or, even worse, simply do something by misinterpreting the
meaning of the bits:
@ex[
(eval:error (interp-compile (parse '(add1 #f))))
(interp-compile (parse '(if (zero? #t) 7 8)))
]

@;codeblock-include["dupe/correct.rkt"]

This complicates testing the correctness of the compiler.  Consider
our usual appraoch:
@ex[
(define (check-correctness e)
  (check-equal? (interp-compile e)
                (interp e)
                e))

(check-correctness (parse '(add1 7)))
;;(eval:error (check-correctness (parse '(add1 #f))))
]

This isn't a counter-example to correctness because @racket['(add1
#f)] is not meaningful according to the semantics.  Consequently the
interpreter and compiler are free to do anything on this input.

Since we know Racket will signal an error when the interpreter tries
to interpret a meaningless expression, we can write an alternate
@racket[check-correctness] function that catches any exceptions and
produces void, effectively ignoring the test:

@ex[
(define (check-correctness e)
  (with-handlers ([exn:fail? void])
    (check-equal? (interp-compile e)
                  (interp e)
                  e)))

(check-correctness (parse '(add1 7)))
(check-correctness (parse '(add1 #f)))
]

Using this approach, we check the equivalence of the results only when
the interpreter runs without causing an error.
