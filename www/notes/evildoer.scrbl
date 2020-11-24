#lang scribble/manual

@(require (for-label (except-in racket ... compile)))
@(require redex/pict
          racket/runtime-path
          scribble/examples
          "../fancyverb.rkt"
	  "utils.rkt"
	  "ev.rkt"	  
	  "../utils.rkt")

@(define (shellbox . s)
   (parameterize ([current-directory (build-path notes "evildoer")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))


@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "evildoer" f))))))
	   '("interp.rkt" "interp-io.rkt" "compile.rkt" "ast.rkt" "parse.rkt" "asm/interp.rkt" "asm/printer.rkt"))


@title[#:tag "Evildoer"]{Evildoer: change the world a couple nibbles at a time}

@emph{Warning: Side effects may include itching, burning,
 oozing, weeping. Not intended for heart patients and those
 with nervous disorders.}

@table-of-contents[]

@verbatim|{
TODO:
* explain compilation of `begin'
* explain push, pop, call
* explain ABI for calling C
* explain "extern" in ASM
* discuss asm-interp/io as analog of interp-io
* show examples with asm-interp/io, asm-interp only for "pure" programs
* add peek-byte?
}|


@section{Reading and writing bytes}

So far, the languages we've consider have had the following
property: the result of evaluation is determined entirely as
a function of the expression being evaluated. Which is to
say, the meaning of a program is determined entirely by the
text of the program. This is a nice property in that it
makes reasoning about programs pretty clear cut: if you know
the program, you can know exactly what it computes. However,
many real-world programs (like the very compiler we are
writing!) do not have this property. Instead they interact
with the outside world and compute results based on the
state of the world.

For example, consider the @tt{compile-file.rkt} program,
which reads the contents of a file from disk and compiles
it. The meaning of this program depends on the state of your
computer's hard drive. Similarly, it prints out assembly
code to the standard output port. So not only does this
program depend on the outside world, it changes it too.

Let's design a language that has a simple mechanism for
interacting with the outside world. It will be able to read
and write a byte of information at a time (i.e. an integer
between 0 and 256) from the standard input port and output
port, respectively.

We'll call it @bold{Evildoer}.

To the syntax of expressions, we add the following operations:

@itemlist[
 @item{@racket[write-byte] @tt|{: Byte -> Void}|: writes given byte to stdout, produces nothing.}
 @item{@racket[read-byte] @tt|{: -> Byte or EOF}|: reads a byte from stdout, if there is one, EOF otherwise.}
 ]

These operations will behave like their Racket counterpart.

To complement these operations, we add two new values:

@itemlist[
  @item{void: a value used to indicate something has been done for effect only and has no useful result.}
  @item{eof: a value used to indicate the end of an input port has been reached.}
 ]

In order to recognize the end-of-file value, we add the following predicate (just as in Racket):

@itemlist[
 @item{@racket[eof-object?] @tt|{: Any -> Boolean}|: determines if argument is the eof value.}
 ]


Abstract syntax and parsing is done as you would expect.  Since we now have a primitive operation
that takes 0 argument, we split the @racket[Prim] constructor into @racket[Prim0] and @racket[Prim1].

@codeblock-include["evildoer/ast.rkt"]

The s-expression parser is defined as follows:

@codeblock-include["evildoer/parse.rkt"]

@section{Reading and writing bytes in Racket}


Racket has a Byte data type that is @emph{not} disjoint from
other datatypes; it's simply an integer in @math{(0,256]}.
The operations @racket[read-byte] and @racket[write-byte]
read and write, respectively, a byte to or from stdin or
stdout (by default).

Let's look at an example of @racket[write-byte]:

@ex[
(write-byte 97)
(write-byte 109)
]

A byte, when written corresponds to an ASCII character,
which is why you see @tt{a} for 97 and @tt{m} for 109.

A subtle, but crucial point here that these expressions are
@emph{printing}, i.e. writing bytes to stdout. But they
don't @emph{produce} any value. Or more precisely, they
print and then produce a value that indicates "no useful
value has been produced." In OCaml, this value is called
"unit"; in Racket, it's called "void." When the REPL is
given an expression that produces void as it's result, it
doesn't show anything. Here's any example that uses the
Racket @racket[void] function, which simply returns the void
value:

@ex[
(void)
(+ 2 3)
(void)
(void)
"fred"
]

It's important to note that void is just a value like any
other value; it's not literally "nothing." It's just handled
specially by the REPL in this case by not showing anything.
Were we to put the void value within another value, the REPL
shows it:

@ex[
 (define xs (list (void) (void) 3 (void)))
 xs
 (length xs)
 (first xs)
 (void? (first xs))
]

So what @racket[write-byte] is doing is printing something
and producing void. If we were to sequence
@racket[write-byte] using @racket[begin], would print
something and produce a non-void value:

@ex[
(begin (write-byte 97)
       #t)
]

Notice how the REPL in the notes is helpful using color to
distinguish the printed output from the program and the
result of the program.

Now's let's look at @racket[read-byte]. It takes no
arguments and reads a byte from stdin. If there's no more
bytes on stdin, it produces @racket[eof].

Now, making examples of @racket[read-byte] is a bit more
tricky. While @racket[write-byte] interacts with the outside
world by printing something to stdout, what it prints is
determined by the program text. On the other hand, what
@racket[read-byte] reads depends on what's in the stdin
stream. If you launch Racket and type @racket[(read-byte)],
Racket will then block, waiting for you to type something,
so that a byte can be read. It will produce whatever the
first byte of what you type is.

So how can I make examples?

One option is to use the operating system shell to ``pipe''
output from one program as input to the Racket process,
which will then read that data as it's input. Together with
@tt{printf} program, we can write the data we want the
Racket program to see on stdin:

@shellbox["printf 'hello' | racket -e '(read-byte)'"]
@shellbox["printf 'hello' | racket -e '(list (read-byte) (read-byte))'"]

If we pipe the empty string, the program will produce the
@racket[eof] value:

@shellbox["printf '' | racket -e '(read-byte)'"]

Another possibility is to use a similar mechanism but from
within Racket. The @racket[with-input-from-string] uses a
given string as the data available on stdin. Then
@racket[(read-byte)] will read from this data:

@ex[
 (with-input-from-string "hello" (λ () (read-byte)))
 (with-input-from-string "hello" (λ () (list (read-byte) (read-byte))))
 (with-input-from-string "" (λ () (read-byte)))
]

This uses @racket[with-input-from-string] which takes a
string and a zero-argument function. It then installs the
contents of the string in stdin as though you had typed this
data, then invokes the function, thereby running a
computation with a predetermined input stream.

There's a matching @racket[with-output-to-string] function
that takes a zero-argument function and runs it in a way
that collects the output and produces it as a string. This
let's you capture what was printed and turn it in to a value
that is produced:

@ex[
(with-output-to-string (λ () (write-byte 104)))
]

These facilities will be useful for making examples, but also
for writing test cases, since we can set up the state of
the outside world and capture changes as values, which we
can then use to assert the expected behavior:

@ex[
 (check-equal?
  (with-input-from-string "hello"
    (λ ()
      (with-output-to-string
        (λ ()
          (write-byte (read-byte))))))
  "h")]


@section{Meaning of Evildoer programs}

(Semantics ommitted for now.)

Here's an interpreter for Evildoer, that relies on the
underlying implementations @racket[read-byte],
@racket[write-byte], etc. from Racket:

@codeblock-include["evildoer/interp.rkt"]

Interpreting a program that reads and writes will itself
read and write:

@ex[
 (interp (parse '(write-byte 104)))
 (with-input-from-string "hello"
   (λ ()
     (interp (parse '(write-byte (read-byte))))))
 ]

We can also build a useful utility for interpreting programs
with strings representing stdin and stdout:

@codeblock-include["evildoer/interp-io.rkt"]

@ex[
 (interp/io (parse '(write-byte 104)) "") 
 (interp/io (parse '(write-byte (read-byte))) "hello")
 ]

This is useful to write tests about programs that have side-effects:

@ex[
 (check-equal? (interp/io (parse '(write-byte (read-byte))) "hello")
               "h")
 ]


@section{A Comiler for Evildoer}

We add new bit encodings for @racket[eof] and @racket[void]:

@filebox-include[fancy-c "evildoer/types.h"]

The compiler:

@codeblock-include["evildoer/compile.rkt"]

@section{A Run-Time for Evildoer}

The run-time system implements @racket[read_byte] and
@racket[write_byte] in C:

@filebox-include[fancy-c "evildoer/byte.c"]

The main run-time file is extended slightly to take care of
printing the new kinds of values (eof and void). Note that a
void result causes nothing to be printed:

@filebox-include[fancy-c "evildoer/main.c"]

