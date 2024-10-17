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
@(ev `(current-directory ,(path->string (build-path langs "mug"))))
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))
@(void (ev '(current-objs '("runtime.o"))))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '("interp.rkt" "compile.rkt" "compile-expr.rkt" "compile-literals.rkt" "utils.rkt" "ast.rkt" "parse.rkt" "types.rkt"))

@(define this-lang "Mug")

@title[#:tag this-lang]{@|this-lang|: symbols and interned string literals}

@src-code[this-lang]

@table-of-contents[]

@section[#:tag-prefix "mug"]{String Literals}

As it currently stands in our language, @bold{string literals} are
dynamically allocated when they are evaluated.

This means, for example, that if we had a program like this:

@#reader scribble/comment-reader
(racketblock
(define (f) "fred")
(cons (f) (cons (f) (cons (f) '())))
)

This will allocate three distinct copies of the string
@racket["fred"], one for each call to @racket[f].  This is unfortunate
since really just a single allocation of @racket["fred"] that is
referenced three times could've worked just as well and allocated less
memory.

A common approach programming language implementations take is to take
every string literal that appears in a program and all allocate it
@bold{once} and replace occurrences of those literals with references
to memory allocated for it.

This means, for example, that multiple occurrences of the same string
literal evaluate to the same pointer:

@ex[
(eq? "x" "x")
]

Note that this doesn't mean that every string of the same characters
is represented by a unique pointer.  We can dynamically construct
strings that will not be equal to a string literal of the same
characters:

@ex[
(eq? "x" (string #\x))
]

Let's consider how strings were previously compiled.  Here's an assembly program
that returns @racket["Hello!"]:

@ex[
(require loot/compile)
(seq (Label 'entry)
     (Mov 'rbx 'rdi)
     (compile-string "Hello!")
     (Ret))
]

We can run it just to make sure:

@ex[
(bits->value
 (asm-interp
  (seq (Global 'entry)
       (Label 'entry)
       (Mov 'rbx 'rdi)
       (compile-string "Hello!")
       (Ret))))
]

Notice that this program dynamically allocates the string by executing
instructions that write to memory pointed to by @racket['rbx] and
incrementing @racket['rbx].

But fundamentally, we shouldn't need to do anything dynamically if we
know statically that the string being return is @racket["Hello!"].  We
could @emph{statically} allocate the memory for the string at
compile-time and return a pointer to this data.

@section[#:tag-prefix "mug"]{Static Memory}

How can we statically allocate memory?  The idea is to use memory in
the program itself to store the data needed to represent the string
literal.  It turns out that in an a86 program you can have a section
for the program text and another section with binary data.  To switch
between the program text and program data, we use the @racket[(Text)]
and @racket[(Data)] directive.  Once in @racket[(Data)] mode we can
write down data that will be placed in the program.

For example, here is a data section:

@ex[
(seq (Data)
     (Label 'hi)
     (Dq 6)
     (Dd (char->integer #\H))
     (Dd (char->integer #\e))
     (Dd (char->integer #\l))
     (Dd (char->integer #\l))
     (Dd (char->integer #\o))
     (Dd (char->integer #\!)))

]

These psuedo-instructions will add to the data segment of our program
56-bytes of data.  The first 8-bytes consist of the number 6.  The
next 4-bytes consist of the number @racket[72], i.e. the codepoint for
@racket[#\H].  The next 4-bytes consist of the codepoint for
@racket[#\e] and so on.  The names of these psuedo-instructions
designate how much memory is used: @racket[Dq] means 8-bytes
(64-bits), while @racket[Dd] means 4-bytes (32-bits).

The label @racket['hi] is given to name this data's location.  We've
previously seen how to load the address of a label using the
@racket[Lea] instruction in order to compute a place in the code to
jump to.  Similarly, if we load the address of @racket['hi], we have a
pointer to the data at that location in the program.

So to write a similar program that returns @racket["Hello!"] but
@emph{statically} allocates the memory for the string, we could do the
following:

@ex[
(bits->value
 (asm-interp
  (seq (Global 'entry)
       (Label 'entry)
       (Lea 'rax 'hi)
       (Or 'rax type-str)
       (Ret)
       (Data)
       (Label 'hi)
       (Dq 6)
       (Dd (char->integer #\H))
       (Dd (char->integer #\e))
       (Dd (char->integer #\l))
       (Dd (char->integer #\l))
       (Dd (char->integer #\o))
       (Dd (char->integer #\!)))))
]

A couple things to note:
@itemlist[

@item{nothing is allocated in the heap memory set up by the run-time;
indeed this program doesn't use the @racket['rbx] register at all.}

@item{Executing this program takes fewer steps than the previous
version; when the @racket['entry] label is called, it executes an
@racket[Lea] and @racket[Or] instruction and returns.}

]

This is pretty big improvement over the previous approach since the
number of instructions to execute were proportional to the size of the
string being compiled.  Now we simply load the address of the static
data in a small, constant number of instructions.

In fact, we can do one better.  The @racket[Or] instruction is there
in order to tag the pointer to @racket['hi] as a string.  There's
really no reason to do this at run-time; we should be able to add the
tag statically so that just a single load instruction suffices.  The
goal is to add the tag to the address of @racket['hi] at compile time,
but the location of the label is actually not fully known until link
time.  Our assembler has a way of resolving this by allowing us to
write @emph{expressions} involving labels and constants that will be
computed at link time.

Here is a version of the same program that avoids the @racket[Or]
instruction, instead computing that type tagging at link time:

@ex[
(bits->value
 (asm-interp
  (seq (Global 'entry)
       (Label 'entry)
       (Lea 'rax (Plus 'hi type-str))
       (Ret)
       (Data)
       (Label 'hi)
       (Dq 6)
       (Dd (char->integer #\H))
       (Dd (char->integer #\e))
       (Dd (char->integer #\l))
       (Dd (char->integer #\l))
       (Dd (char->integer #\o))
       (Dd (char->integer #\!)))))
]


So one idea is to use static data to represent string literals.  This
reduces the run-time memory that is allocated and makes is more
efficient to evaluate string literals.  We could replace the old
@racket[compile-string] function with the following:

@ex[
(define (compile-string s)
  (let ((l (gensym 'string)))
    (seq (Data)
	 (Label l)
	 (Dq (string-length s))
	 (map Dd (map char->integer (string->list s)))
	 (Text)
	 (Lea 'rax (Plus l type-str)))))

(compile-string "Hello!")

(bits->value
 (asm-interp
  (seq (Global 'entry)
       (Label 'entry)
       (compile-string "Hello!")
       (Ret))))
]

Now, while this does allocate string literals statically, using memory
within to the program to store the string, it doesn't alone solve the
problem with string literals being represented uniquely.

@section[#:tag-prefix "mug"]{Static Interning}

We've seen static memory, but we still need to make sure every string
literal is allocated just once.

Here is the basic idea:

@itemlist[

@item{Collect all of the string literals in the program.}

@item{For each distinct string literal, compile it to static data as
described above, labelling the data location.}

@item{For each string literal expression, compile it to a reference to
the appropropiate label for that string.}

]

For example, let's say we want to compile this program:

@#reader scribble/comment-reader
(racketblock
(begin "Hello!"
       "Hello!")
)

We'd like it to compile to something like this:

@#reader scribble/comment-reader
(racketblock
(seq (Mov 'rax (Add 'hi type-str))
     (Mov 'rax (Add 'hi type-str))
     (Ret)
     (Data)
     (Label 'hi)
     (Dq 6)
     (Dd (char->integer #\H))
     (Dd (char->integer #\e))
     (Dd (char->integer #\l))
     (Dd (char->integer #\l))
     (Dd (char->integer #\o))
     (Dd (char->integer #\!)))
)

Notice how the two occurrences of @racket["Hello!"] turn into the
instruction @racket[(Mov 'rax (Add 'hi type-str))].  The labelled
location @racket['hi] contains the data for the string and it is
statically allocated just once.

In order to do this, we need to maintain an association between unique
string literals and the labels our compiler will choose to label their
static data.

We @emph{could} do this by making a pass over the program to compute
this association.  Initially it would be empty and every time a string
literal was encountered, we'd check to see if it's already in the
association.  If it is, there's nothing to be done.  If isn't, we'd
generate a new label and add it to the association.

This association would have to be added as a parameter to each of our
@racket[compile-e] functions and string literals would consult the
association to emit the @racket[(Mov 'rax (Add _label type-str))]
instruction.

We'd also take every label and string pair in the association and
compile the string data to static data labelled with the associated
label.

However, here's a fun ``trick'' we can employ to avoid having to
explicitly represent this association between strings and their
labels.

Strings can be converted to symbols, and symbols can be used as
labels.  Symbols that consist of the same characters are guaranteed to
be pointer-equal to each other, so by converting a string to a symbol,
we can take advantage of our implementation language's (Racket's)
facility for interning to help us implement interning in our compiler.

So here is our revised apporach will produce code like this for our
example program:

@#reader scribble/comment-reader
(racketblock
(seq (Mov 'rax (Add (symbol->label (string->symbol "Hello!")) type-str))
     (Mov 'rax (Add (symbol->label (string->symbol "Hello!")) type-str))
     (Ret)
     (Data)
     (Label (symbol->label (string->symbol "Hello!")))
     (Dq 6)
     (Dd (char->integer #\H))
     (Dd (char->integer #\e))
     (Dd (char->integer #\l))
     (Dd (char->integer #\l))
     (Dd (char->integer #\o))
     (Dd (char->integer #\!)))
)

So now an occurrence of a string literal @racket[_str] can be compiled
as @racket[(Mov 'rax (string->label (string->symbol _str)))]; no
association needs to be maintained explicity.

@#reader scribble/comment-reader
(racketblock
;; String -> Asm
(define (compile-string s)
  (seq (Lea 'rax (Plus (symbol->label (string->symbol s)) type-str))))
)

@(ev '(define (compile-string s)
	(seq (Lea 'rax (Plus (symbol->label (string->symbol s)) type-str)))))

So here's how an occurrence of @racket["Hello!"] is compiled:

@ex[
(compile-string "Hello!")
]

We still need to compile the set of string literals that appear in the
program into statically allocated data, so for this we will write a
function:

@#reader scribble/comment-reader
(racketblock
;; Prog -> [Listof Symbol]
(define (literals p) ...)
)

This will produce the set of strings that appear literally in the
program text.  Each string will be converted to its symbol
representation.  The string representation is easy to recover by using
@racket[symbol->string].

This function is straightforwad, if a bit tedious, to write.  It
traverses the AST.  Recursive results are collected with
@racket[append]; when a string node @racket[(Str _s)] is encountered,
it produces @racket[(list (string->symbol _s))].  After all of the
strings have been collected, a final call to
@racket[remove-duplicates] ensures a list of unique symbols is
returned.

@ex[
(literals (parse '["Hello!"]))
(literals (parse '[(begin "Hello!" "Hello!")]))
(literals (parse '[(begin "Hello!" "Fren")]))
(literals (parse '[(define (f x) "Hello!")
		   (cons (f "Fren") (cons (f "Hello!") '()))]))
]

Using @racket[literals], we can write a function that compiles all of
the string literals into static data as follows:

@(ev '(require mug/compile-literals))

@#reader scribble/comment-reader
(ex
;; Prog -> Asm
(define (compile-literals p)
  (append-map compile-literal (literals p)))

;; [Listof Char] -> Asm
(define (compile-string-chars cs)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Dd (char->integer c))
          (compile-string-chars cs))]))

;; Symbol -> Asm
(define (compile-literal s)
  (let ((str (symbol->string s)))
    (seq (Label (symbol->label s))
         (Dq (string-length str))
         (compile-string-chars (string->list str))
         (if (odd? (string-length str))
             (seq (Dd 0))
             (seq)))))

(seq (compile-string "Hello!")
     (compile-string "Hello!")
     (compile-literal 'Hello!))
)

We've seemingly reached our goal.  However, there is a fairly nasty
little bug with our approach.  Can you spot it?

Here's a hint: we are generating labels based on the content of string
literals.  What else do we generate labels based off of and is it
possible to create a conflict?

The answer is yes.  Consider this program:

@#reader scribble/comment-reader
(racketblock
(define (Hello! x) "Hello!")
42
)

It contains both a function called @racket[Hello!] and a string
literal @racket["Hello!"].  Unfortunately, the label used both for the
function and the string data will be @racket[(symbol->label 'Hello!)].
If the compiler emits two definitions of this label, the assembler
will complain and fail to assemble the program.

The solution is simple, when generating labels for data, we will use a
different symbol to label function, let's call it
@racket[symbol->data-label] that is guaranteed to produce disjoint
labels from @racket[symbol->label], which we will continue to use for
code labels.

Using this function in all the places we used @racket[symbol->label]
will resolve the issue and our problematic program will now have two
different labels defined in it:

@ex[
(symbol->label 'Hello!)
(symbol->data-label 'Hello!)
]


So now we have accomplished our goal: string literals are statically
allocated and different occurrence of the same string literal are
considered @racket[eq?] to each other:

@(ev '(require mug/compile))

@ex[
(seq (compile-string "Hello!")
     (compile-string "Hello!")
     (compile-literal 'Hello!))
]

We can try it out to confirm some examples.


@ex[
(define (run . p)
  (bits->value (asm-interp (compile (parse p)))))

(run "Hello!")

(run '(begin "Hello!" "Hello!"))

(run '(eq? "Hello!" "Hello!"))
(run '(eq? "Hello!" "Fren"))

(run '(define (Hello! x) "Hello!")
     '(eq? (Hello! 42) "Hello!"))
]

It's still worth noting that only string literals are interned.
Dynamically created strings are not pointer-equal to structurally
equal string literals:

@ex[
(run '(eq? "fff" (make-string 3 #\f)))
]

This is why we refer to this kind of interning as ``static'' interning.

Let us now turn to a new, but familar, data type that supports a
stronger sense of interning: the symbol.

@section[#:tag-prefix "mug"]{Symbols}

One basic data type that we've used frequently in the writing of our
compiler, but which is not currently accounted for in our language is
that of @bold{symbols}.

At first cut, a symbol is alot like a string: the name of a symbol
consists of some textual data.  We can represent a symbol much like we
represent a string: using a tagged pointer to a sized array of
characters that comprise the name of the symbol.

In fact, we made extensive use of this in our implementation of static
interning for string literals.  This section will now uncover
@emph{how} symbols do their (dynamic) interning.

From a syntax point of view, we add a new AST constructor for symbols
and names of the new operations:

@filebox-include-fake[codeblock "mug/ast.rkt"]{
;; type Expr = ...
;;           | (Symb Symbol)
;; type Op1  = ...
;;           | 'symbol? | 'symbol->string
;;           | 'string->symbol | 'string->uninterned-symbol
(struct Symb (s) #:prefab)
}

The parser is updated to construct such AST nodes when it encounters a
symbol:

@ex[
(parse-e ''foo)
]

We can create a new pointer type tag:

@filebox-include-fake[codeblock "mug/types.rkt"]{
(define type-symb #b110)
}

The run-time system has to be updated to handle symbol results and the
printer is updated to properly print symbols, but all of this follows
the blueprint of strings.  It's simply a different tag and a slightly
different printer, which uses and initial @tt{'} delimiter instead of
an initial @tt{"} and subsequent @tt{"} delimiter.

But one of the key differences between strings and symbols is that
symbols that have the same name are considered the same, i.e. they
should be represented by the same @emph{pointer}.

This means that two symbols of the same name should be @racket[eq?] to
each other:

@ex[
(eq? 'x 'x)
]

Having seen how string literals are handled, you can see that symbol
literals are like string literals and we can take a similar approach
to transform a program into one that statically allocates all of the
symbols that appear in the program and replace their occurrences with
references.

Again, we just follow the blueprint of strings.

The key additions are a function for compiling symbol occurrences:

@#reader scribble/comment-reader
(racketblock
;; Symbol -> Asm
(define (compile-symbol s)
  (seq (Lea 'rax (Plus (symbol->data-label s) type-symb))))
)

Which works as follows:

@ex[
(compile-symbol 'Hello!)
]

And the @racket[literals] function should now include a case for
@racket[(Symb _sym)] to return @racket[(list _sym)].

@ex[
(literals (parse '['Hello!]))
]

You might worry that programs that have similar strings and symbols
may cause problem.  Since @racket[literals] on the following program
only returns a single literal:

@ex[
(literals (parse '[(begin "Hello!" 'Hello!)]))
]

But actually this is just fine.  What happens is that only a single
chunk of memory is allocated to hold the character data @tt{H},
@tt{e}, @tt{l}, @tt{l}, @tt{o}, @tt{!}, but the @emph{symbol}
@racket['Hello] is represented as a pointer to this data tagged as a
symbol, while the string @racket["Hello"] is represent as the same
pointer, but tagged as a string.  So this program compiles to:

@ex[
(seq (compile-string "Hello!")
     (compile-symbol 'Hello!)
     (compile-literal 'Hello!))
]

We have now added a symbol data type and have implement static
interning just as we did for strings.

However this strategy alone won't fully solve the problem of symbol
identity because it is possible to dynamically create symbols and even
then it should be the case that symbols with the same name are ``the
same.''  This in contrast to how strings work:

@ex[
(eq? 'x (string->symbol (string #\x)))
]

Here we are creating a symbol dynamically, using the string
@racket["x"] to specify the name of the symbol.  Comparing it to a
@racket['x] that appears statically should still produce @racket[#t].

This was in fact a critical property we relied upon in implementing
static string interning.

This latter example shows that we need to @emph{dynamically} ensure
symbols of the same name evaluate to unique pointers.

@section[#:tag-prefix "mug"]{Dynamic Interning}

Static interning requires identical static occurrences of data to have
a unique representation.  Dynamic interning requires identical data,
regardless of when it's created, to have a unique representation.
Symbols are like strings that support dynamic interning.

This is going to require more support from our run-time system.

Essentially, the run-time systems needs to keep track of all of the
symbols that have appeared so far during the running of the program.
When a new symbol is dynamically created, e.g. through
@racket[string->symbol], the run-time will check whether this symbol
has been seen before (based on the characters of its name).  If it has
been seen before, the run-time can give us the pointer for the
previous use of the symbol, thus preserving the pointer-equality
between this symbol and any other occurrences.

On the other hand if the run-time has not see this symbol, it can
allocate memory for it, return the pointer, and remember in the future
that this symbol has been seen.

To accomplish this, we will implement a @bold{symbol table}.  It
associates symbol names, i.e. the characters of a symbol, with
pointers to symbols.  When a program wishes to create a symbol, it
confers with the table to either fetch an existing pointer for the
symbol or create a new on, updating the table.

To implement this table, we'll use a binary search tree of symbols,
represented in C as.  We have a globally defined pointer
@tt{symbol_tbl} is which is initially empty (@tt{NULL}).  The work of
dynamically interning a symbol will be done by the @tt{intern_symbol}
function.  It searches the BST, using @tt{symb_cmp} to compare symbols
for alphabetic ordering.  If an entry is found, it returns the
previously seen symbol, otherwise it adds the symbol to the table and
returns it.

@filebox-include[fancy-c mug "symbol.c"]

The idea will be that every time a symbol is constructed, we call
@tt{intern_symbol} to intern it.

So in addition to collecting all of the literals and compiling each to
static data, we will need to collect all of the symbols and emit a
call to @tt{intern_symbol} at the start of the program.

To accomplish this, we'll design a function:

@#reader scribble/comment-reader
(racketblock
;; Prog -> Asm
;; Initialize the symbol table with all the symbols that occur statically
(define (init-symbol-table p) ...)
)

Here's what it will produce for some example programs:

@ex[
(init-symbol-table (parse '['Hello!]))
(init-symbol-table (parse '[(begin 'Hello! 'Hello!)]))
(init-symbol-table (parse '["Hello!"]))
(init-symbol-table (parse '[(define (Hello! x) 'Hello!)
			    (Hello! 'Fren)]))
]

For each unique symbol in the program, it emits two instructions:

@itemlist[

@item{move the address of the symbol's data into @racket['rdi], the
register used for the first argument in the System V ABI,}

@item{call @tt{intern_symbol}.}
]

We know that initially the table is empty, so each of these calls will
insert the given symbols into the table ensure that if any subsequent
symbol is interned that has the same character data, call
@tt{intern_symbol} will produce the original pointer to static data
for that symbol.

Now we can implement the two operations @racket[string->symbol] and
@racket[symbol->string].  Here's what we do for
@racket[string->symbol]:

@#reader scribble/comment-reader
(racketblock
;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ; ...
    ['string->symbol
     (seq (assert-string rax)
	  (Xor rax type-str)
	  (Mov rdi rax)
	  pad-stack
	  (Call 'intern_symbol)
	  unpad-stack
	  (Or rax type-symb))]))
)

This first does some type-tag checking to make sure the argument is a
string, then it untags the pointer and moves it to the @racket['rdi]
register in order to call @racket[intern_symbol].  The address of the
interned symbol is returned in @racket['rax], which is then tagged as
being a symbol.

We can now confirm that dynamically created symbols are still
pointer-equal to symbols that statically appear in the program:

@ex[
(run '(eq? 'fff (string->symbol (make-string 3 #\f))))
]

Even creating two symbols dynamically will result in the same pointer
so long as they are spelled the same:

@ex[
(run '(eq? (string->symbol (make-string 3 #\a))
	   (string->symbol (make-string 3 #\a))))
]

Going the other direction from symbols to strings is easy: we copy the
string data and tag the pointer as a string.  Note that we could get
away will simply retagging the pointer and not actually copying the
string, but we make a copy to mimic Racket's behavior and to be safe
should we add string mutation operations.

@#reader scribble/comment-reader
(racketblock
;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ; ...
    ['symbol->string
     (seq (assert-symbol rax)
	  (Xor rax type-symb)
	  char-array-copy
	  (Or rax type-str))]))

;; Asm
;; Copy sized array of characters pointed to by rax
(define char-array-copy
  (seq (Mov rdi rbx)            ; dst
       (Mov rsi rax)            ; src
       (Mov rdx (Offset rax 0)) ; len
       (Add rdx 1)              ; #words = 1 + (len+1)/2
       (Sar rdx 1)
       (Add rdx 1)
       (Sal rdx 3)              ; #bytes = 8*#words
       pad-stack
       (Call 'memcpy)
       unpad-stack
       (Mov rax rbx)
       (Add rbx rdx)))
)

The @racket[char-array-copy] sequence of instructions sets up a call
to C's @tt{memcpy} function giving the address of the string data as
the source, the current heap pointer as the destination, and the
number of bytes which will be copied.  After the call returns, the
heap pointer is incremented by that number of copied bytes.

We can see that this works:

@ex[
(run '(symbol->string 'foo))
]

To observe the copying behavior, notice:

@ex[
(run '(eq? (symbol->string 'foo) (symbol->string 'foo)))
]


@section[#:tag-prefix "mug"]{Uninterned Symbols}

Sometimes it is useful to create a symbol that is distinct from all
other symbols.  We've relied on the ability to create a symbol with
this property whenever we used the @racket[gensym] operation.  What
@racket[gensym] produces is an @bold{uninterned} symbol.  Even if you
constructed a symbol with the same letters, it would be a different
pointer from the symbol created by a call to @racket[gensym].

To add this ability, we will add an precursor to @racket[gensym]
called @racket[string->uninterned-symbol].  It consumes a string and
produces a symbol with the same letters, but distinct from all other
symbols, even those that are spelled the same.

@ex[
(eq? 'Hello! (string->uninterned-symbol "Hello!"))
]

Calling @racket[string->uninterned-symbol] twice with the same string
will produce two different symbols:

@ex[
(eq? (string->uninterned-symbol "Hello!")
     (string->uninterned-symbol "Hello!"))
]

Implementing @racket[string->uninterned-symbol] is fairly simple: we
allocate a new symbol, thereby ensuring it is unique and then simple
avoid calling @tt{intern_symbol}:

@#reader scribble/comment-reader
(racketblock
;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ; ...
    ['string->uninterned-symbol
     (seq (assert-string rax)
	  (Xor rax type-str)
	  char-array-copy
	  (Or rax type-symb))]))
)

We can confirm this works as expected:

@ex[
(run '(string->uninterned-symbol "foo"))
(run '(eq? 'foo (string->uninterned-symbol "foo")))
(run '(eq? (string->uninterned-symbol "foo")
           (string->uninterned-symbol "foo")))
]

With that, we have completed the implementation of symbols and strings
with the proper interning behavior.


@section[#:tag-prefix "mug"]{Matching symbols and strings}

Since we have @racket[match] in our language, we should probably add
the ability to match against strings and symbols.

We can extend the AST definition for patterns:

@filebox-include-fake[codeblock "mug/ast.rkt"]{
;; type Pat = ...
;;          | (PSymb Symbol)
;;          | (PStr String)
(struct PSymb (s) #:prefab)
(struct PStr (s)  #:prefab)
}

Extending the interpreter is straightforward:

@filebox-include-fake[codeblock "mug/interp.rkt"]{
;; Pat Value Env -> [Maybe Env]
(define (interp-match-pat p v r)
  (match p
    ; ...
    [(PSymb s) (and (eq? s v) r)]
    [(PStr s)  (and (string? v) (string=? s v) r)]))
}

Extending the compiler is more involved, but essentially boils down to
doing exactly what the interpreter is doing above:

@filebox-include-fake[codeblock "mug/compile-expr.rkt"]{
;; Pat CEnv Symbol -> (list Asm Asm CEnv)
(define (compile-pattern p cm next)
  (match p
    ; ...
    [(PStr s)
     (let ((fail (gensym)))
       (list (seq (Lea rdi (symbol->data-label (string->symbol s)))
                  (Mov r8 rax)
                  (And r8 ptr-mask)
                  (Cmp r8 type-str)
                  (Jne fail)
                  (Xor rax type-str)
                  (Mov rsi rax)
                  pad-stack
                  (Call 'symb_cmp)
                  unpad-stack
                  (Cmp rax 0)
                  (Jne fail))
             (seq (Label fail)
                  (Add rsp (* 8 (length cm)))
                  (Jmp next))
             cm))]
    [(PSymb s)
     (let ((fail (gensym)))
       (list (seq (Lea r9 (Plus (symbol->data-label s) type-symb))
                  (Cmp rax r9)
                  (Jne fail))
             (seq (Label fail)
                  (Add rsp (* 8 (length cm)))
                  (Jmp next))
             cm))]))
}

The implementation of string matching uses the @tt{symb_cmp} function
from the run-time system, checking whether it returns @racket[0] to
indicate the strings are the same.  (Although the function is
concerned with comparing symbols, symbols and strings are represented
the same, so it works just as well to compare strings.)

We can confirm some examples:

@ex[
(run '(match 'foo
        ['foo  1]
        ["foo" 2]))
(run '(match "foo"
        ['foo  1]
        ["foo" 2]))
(run '(match (cons '+ (cons 1 (cons 2 '())))
        [(cons '+ (cons x (cons y '())))
	 (+ x y)]))
]


@section[#:tag-prefix "mug"]{Compiling Symbols and Strings}

We can now put the pieces together for the complete compiler.

@(define (code-link fn)
   (link (string-append "code/" fn) (tt fn)))

We do a bit of housekeeping and move the code for compiling
expressions to its own module: @code-link{mug/compile-expr.rkt}.


The top-level compiler is now:

@filebox-include[codeblock mug "compile.rkt"]

The work of compiling literals and emitting calls to initialize the
symbol table is contained in its own module:

@filebox-include[codeblock mug "compile-literals.rkt"]
