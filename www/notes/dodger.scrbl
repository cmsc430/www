#lang scribble/manual

@(require (for-label (except-in racket ... compile) a86))
@(require redex/pict
          racket/runtime-path
          scribble/examples
          "../fancyverb.rkt"
	  "utils.rkt"
	  "ev.rkt"	  
	  "../utils.rkt")



@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "dodger" f))))))
	   '("interp.rkt" "compile.rkt" "ast.rkt" "parse.rkt" "types.rkt"))


@title[#:tag "Dodger"]{Dodger: addressing a lack of character}

@emph{There are 11 types of values...}

@table-of-contents[]

@section{Characters}

In @secref{Dupe}, we saw how to accomodate disjoint
datatypes, namely integers and booleans. Let's add yet
another: the character type. Conceptually, there's not much
new here (hence we stay in the "D" family of languages);
we're simply adding a third type, which we distinguish from
the other two by using more bits to tag the type.

We'll call it @bold{Dodger}.

To the syntax of expressions, we add character literals.

We will also add the following operations:

@itemlist[
 @item{@racket[char?] @tt|{: Any -> Boolean}|: predicate for recognizing character values}
 @item{@racket[integer->char] @tt|{: Integer -> Character}|: converts from integers to characters}
 @item{@racket[char->integer] @tt|{: Character -> Integer}|: converts from integers to characters}
 ]

Abstract syntax is modelled with the following datatype definition:

@codeblock-include["dodger/ast.rkt"]

The s-expression parser is defined as follows:

@codeblock-include["dodger/parse.rkt"]

@section{Characters in Racket}


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

There are a few other ways to write characters (see the
Racket
@link["https://docs.racket-lang.org/reference/reader.html#%28part._parse-character%29"]{
 Reference} for the details), but you don't have to worry
much about this since @racket[read] takes care of reading
characters in all their different forms. The run-time
system, described below, takes care of printing them.


@section{Meaning of Dodger programs}

The semantics are omitted for now (there's really nothing new that's interesting).

The interpeter is much like that of Dupe, except we have a new base case:

@codeblock-include["dodger/interp.rkt"]

And the interpretation of primitives is extended:

@codeblock-include["dodger/interp-prim.rkt"]

The meaning of characters and their operations are just lifted from Racket.


We can try out some examples:

@ex[
(interp (Char #\a))
(interp (Char #\b))
(interp (Prim1 'char? (Char #\a)))
(interp (Prim1 'char? (Bool #t)))
(interp (Prim1 'char->integer (Char #\a)))
(interp (Prim1 'integer->char (Prim1 'char->integer (Char #\a))))
]

Just as in Dupe, type errors result in the interpreter crashing:


@ex[
(eval:error (interp (Prim1 'char->integer (Bool #f))))
]

Also, not every integer corresponds to a character, so when
@racket[integer->char] is given an invalid input, it crashes
(more on this in a minute):

@ex[
(eval:error (interp (Prim1 'integer->char (Int -1))))
]

@section{Ex uno plures iterum: Out of One, Many... Again}


We have exactly the same problem as in Dupe: we need to
represent different kinds of values within our one
primordial datatype: the 64-bit integer.

We can use the following encoding scheme:

@itemlist[
 @item{Integers have @tt{#b0} as the last bit; the other bits describe the integer.}
 @item{Character have @tt{#b01} as the last bits; the other bits describe the character.}
 @item{True is @tt{#b011} and False is @tt{#b111}.}
]

Notice that each kind of value is disjoint.

We can write an interpreter that operates at the level of bits just as we did for Dupe;
notice that it only ever constructs characters at the very end when converting from
bits to values.  Let's first define our bit encodings:

@codeblock-include["dodger/types.rkt"]

And now the interpreter:

@codeblock-include["dodger/interp-bits.rkt"]


@section{A Compiler for Dodger}

Compilation is pretty easy, particularly since we took the
time to develop the bit-level interpreter. The compiler uses
the same bit-level representation of values and uses logical
operations to implement the same bit manipulating operations.
Most of the work happens in the compilation of primitives:

@codeblock-include["dodger/compile-ops.rkt"]

The top-level compiler for expressions now has a case for character
literals, which are compiled like other kinds of values:

@codeblock-include["dodger/compile.rkt"]

We can take a look at a few examples:

@ex[
(define (show e)
  (displayln (asm-string (compile-e (parse e)))))
(show '#\a)
(show '#\λ)
(show '(char->integer #\λ))
(show '(integer->char 97))]

@section{A Run-Time for Dodger}

The only interesting aspect of Dodger, really, is that we
need to add run-time support for printing character literals.

We extend the bit-encoding of values following the pattern we've
already seen:

@filebox-include[fancy-c "dodger/types.h"]

And update the interface for values in the runtime system:

@filebox-include[fancy-c "dodger/values.h"]
@filebox-include[fancy-c "dodger/values.c"]

The only other change is that @tt{print_result} is updated to handle
the case of printing characters:

@filebox-include[fancy-c "dodger/print.c"]

Will these pieces in place, we can try out some examples:

@ex[
 (define (run e)
   (bits->value (asm-interp (compile (parse e)))))
 (run '#\a)
 (run '(integer->char (add1 (char->integer #\a))))
 (run '(integer->char 955))
]

