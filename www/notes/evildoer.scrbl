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

@(ev '(require rackunit a86))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "evildoer" f))))))
	   '("interp.rkt" "interp-io.rkt" "compile.rkt" "ast.rkt" "parse.rkt"))

@(ev `(current-directory ,(path->string (build-path notes "evildoer"))))
@(void (ev '(system "make byte-shared.o")))

@title[#:tag "Evildoer"]{Evildoer: change the world a couple nibbles at a time}

@emph{Warning: Side effects may include itching, burning,
 oozing, weeping. Not intended for heart patients and those
 with nervous disorders.}

@table-of-contents[]

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
 @item{@racket[peek-byte] @tt|{: -> Byte or EOF}|: peeks a byte from stdout, if there is one, EOF otherwise.}
 ]

These operations will behave like their Racket counterpart.

To complement these operations, we add two new values:

@itemlist[
  @item{void: a value used to indicate something has been done for effect only and has no useful result.}
  @item{eof: a value used to indicate the end of an input port has been reached.}
 ]

The void value arises as the result of an expression that is evaluated
for effect only, such as @racket[write-byte].  The eof value arises as
the result of reading the end of a port.  So that these values can be
accessed more directly, we'll also add:

@itemlist[
  @item{@racket[eof] @tt{: EOF} bound to the end-of-file value, and}
  @item{@racket[void] @tt{: -> Void} a function that produces the void value.}
]


In order to recognize the end-of-file value, we add the following predicate (just as in Racket):

@itemlist[
 @item{@racket[eof-object?] @tt|{: Any -> Boolean}|: determines if argument is the eof value.}
]

Finally, we add a simple sequencing construct so that first
evaluate an expression for effect and then evaluate another
expression for its result.

@itemlist[
 @item{@racket[(begin _e0 _e1)]: evaluates @racket[_e0], then @racket[_e1].}
]


Abstract syntax and parsing is done as you would expect.
Since we now have primitive operations that take 0 argument,
we split the @racket[Prim] constructor into @racket[Prim0]
and @racket[Prim1].

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

Notice how the REPL in the notes is helpfully using color to
distinguish the printed output from the program and the
result of the program.

Now's let's look at @racket[read-byte]. It takes no
arguments and reads a byte from stdin. If there's no more
bytes on stdin, it produces @racket[eof]. Its cousin
@racket[peek-byte] also gets a byte from stdin, but it
leaves the stream intact so that the same byte would be read
the next time around.

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

Formulating the semantics of Evildoer is more complicated
than the languages we've developed so far. Let's put it off
for now and instead focus on the interpreter, which remains
basically as simple as before. The reason for this disparity
is that math doesn't have side-effects. The formal semantics
will need to account for effectful computations without
itself having access to them. Racket, on the other hand, can
model effectful computations directly as effectful Racket
programs.

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

This is useful to write tests about programs that have
side-effects because we can turn what was an effectful
computation into a pure one:

@ex[
 (check-equal? (interp/io (parse '(write-byte (read-byte))) "hello")
               (cons (void) "h"))
 ]

OK, so now, what about the formal mathematical model of
Evildoer? We have to reconsider the domain of program
meanings. No longer does an expression just mean a value;
its meaning may depend upon the state of the input and
output port.  Moreover, an expression may @emph{alter} the
state of these ports.

There are several different approaches we might take to
formally model the effects of @racket[read-byte] and
@racket[write-byte]. We'll adopt a fairly simple one which
is to say that the semantic function now takes and produces
a pair of ports, which we can model as a list of bytes.
Reading from the input port consumes elements from the input
bytes, while writing to the output port appends elements.
This is pretty close in spirit to our @racket[interp/io]
facility, but instead of capturing the effects with string ports,
we will define the meaning of effects directly.

(Semantics omitted for now.)

@section{A Run-Time for Evildoer}

With new values comes the need to add new bit encodings. So
we add new encodings for @racket[eof] and @racket[void]:

@filebox-include[fancy-c "evildoer/types.h"]

The main run-time file is extended slightly to take care of
printing the new kinds of values (eof and void). Note that a
void result causes nothing to be printed:

@filebox-include[fancy-c "evildoer/main.c"]

But the real novelty of the Evildoer run-time is that there
will be new functions that implement @racket[read-byte],
@racket[peek-byte], and @racket[write-byte]; these will be C
functions called @racket[read_byte], @racket[peek_byte] and
@racket[write_byte]:

@filebox-include[fancy-c "evildoer/byte.c"]

The main novely of the @emph{compiler} will be that emits code
to make calls to these C functions.

@section{Calling C functions from a86}

If you haven't already, be sure to read up on how calls work
in @secref{a86}.

Once you brushed up on how calls work, you'll know you can
define labels that behave like functions and call them.

Let's start by assuming we have a simple stand-in for the
run-time system, which is this C program that invokes an
assembly program with a label called @tt{entry} and prints
the result:

@filebox-include[fancy-c "evildoer/print.c"]

Now, here is a little program that has a function called @tt{meaning}
that returns @tt{42}.  The main entry point calls @tt{meaning},
adds 1 to the result, and returns:

@ex[
(define p
  (prog (Label 'entry)
        (Call 'meaning)
        (Add 'rax 1)
        (Ret)
        (Label 'meaning)
        (Mov 'rax 42)
        (Ret)))
]

Let's save it to a file called @tt{p.s}:

@ex[
 (with-output-to-file "p.s"
   (λ ()
     (displayln (asm-string p)))
   #:exists 'truncate)]

We can assemble it, link it together with the printer, and run it:

@(define format (if (eq? (system-type 'os) 'macosx) "macho64" "elf64"))
          
@shellbox["gcc -c print.c -o print.o"
          (string-append "nasm -f " format " p.s -o p.o")
          "gcc print.o p.o -o print"
          "./print"]

In this case, the @tt{meaning} label is defined @emph{within} the same
assembly program as @tt{entry}, although that doesn't have to be the case.
We can separate out the definition of @tt{meaning} into its own file,
so long as we declare in this one that @tt{meaning} is an external label:

@ex[
(define p
  (prog (Extern 'meaning)
        (Label 'entry)
        (Call 'meaning)
        (Add 'rax 1)
        (Ret)))
(define life
  (prog (Label 'meaning)
        (Mov 'rax 42)
        (Ret)))
]

By declaring an external label, we're saying this program
makes use of that label, but doesn't define it. The
definition will come from a later phase where the program is
linked against another that provides the definition.

First, we save each program in its nasm format:

@ex[
(with-output-to-file "p.s"
  (λ ()
    (displayln (asm-string p)))
  #:exists 'truncate)
(with-output-to-file "life.s"
  (λ ()
    (displayln (asm-string life)))
  #:exists 'truncate)]

And assemble:

@shellbox[(string-append "nasm -f " format " p.s -o p.o")
          (string-append "nasm -f " format " life.s -o life.o")]


Then we can link all the pieces together and run it:

@shellbox["gcc print.o p.o life.o -o print"
          "./print"]
          
Now if we look at @tt{life.s}, this is an assembly program
that defines the @tt{meaning} label. We defined it by
writing assembly code, but we could've just as easily
defined it in any other language that can compile to an
object file.  So let's write it in C:


@filebox-include[fancy-c "evildoer/life.c"]

We can compile it to an object file:

@shellbox["gcc -c life.c -o life.o"]

This object file will have a single globally visible label
called @tt{meaning}, just like our previous implementation.
@;{
To confirm this, the standard @tt{nm} utility can be used to
list the defined symbols of an object file:

@shellbox["nm -j life.o"]
}
We can again link together the pieces and confirm that it
still produces the same results:

@shellbox["gcc print.o p.o life.o -o print"
          "./print"]


At this point, we've written a little assembly program (@tt{
 p.s}) that calls a function named @tt{meaning}, that was
written in C.

One thing that you can infer from this example is that the C
compiler generates code for @tt{meaning} that is like the
assembly code we wrote, namely it ``returns'' a value to the
caller by placing a value in @racket['rax].

The next natural question to ask is, how does an assembly
program provide arguments to the call of a C function?

Just as there is a convention that a return value is
communicated through @racket['rax], there are conventions
governing the communication of arguments. The conventions
are known as an @bold{Application Binary Interface} or ABI.
The set of conventions we're following is called the @bold{
 System V} ABI, and it used by Unix variants like Mac OS,
Linux, and BSD systems. (Windows follows a different ABI.)

The convention for arguments is that the first six integer
or pointer parameters are passed in the registers
@racket['rdi], @racket['rsi], @racket['rdx], @racket['rcx],
@racket['r8], @racket['r9]. Additional arguments and large
arguments such as @tt{struct}s are passed on the stack.
                                                     
So now let's try calling a C function that takes a
parameter. Here we have a simple C function that doubles
it's input:

@filebox-include[fancy-c "evildoer/double.c"]

We can compile it to an object file:

@shellbox["gcc -c double.c -o double.o"]

Now, to call it, the assembly program should put the value
of its argument in @racket['rdi] before the call:

@ex[
 (define q
   (prog (Extern 'dbl)
         (Label 'entry)
         (Mov 'rdi 21)
         (Call 'dbl)
         (Add 'rax 1)
         (Ret)))                           
(with-output-to-file "q.s"
  (λ ()
    (displayln (asm-string q)))
  #:exists 'truncate)]

We can assemble it into an object file:

@shellbox[(string-append "nasm -f " format " q.s -o q.o")]

And linking everything together and running shows it works
as expected:

@shellbox["gcc print.o q.o double.o -o print"
          "./print"]


Now we have all the tools needed to interact with libraries
written in C, and really any library object files that
adhere to the System V ABI. Perhaps the only remaining
wrinkle is how should we deal with the situation in which we
are using the registers that are needed to pass parameters
in a call? The answer is to save them on the stack and
restore them when the call returns. For example, suppose
@racket['rdi] held a value we wanted to use after the call
to @tt{dbl}. It's a bit contrived, but let's say we want to
use @racket['rdi] to hold the constant we'll add to the
result of calling @tt{dbl}. Now we need to save it before
writing the argument. All we need to do is add a push and
pop around the call:

@ex[
 (define q
   (prog (Extern 'dbl)
         (Label 'entry)
         (Mov 'rdi 1)
         (Push 'rdi)
         (Mov 'rdi 21)
         (Call 'dbl)
         (Pop 'rdi)
         (Add 'rax 'rdi)
         (Ret)))                           
(with-output-to-file "q.s"
  (λ ()
    (displayln (asm-string q)))
  #:exists 'truncate)]

@shellbox[(string-append "nasm -f " format " q.s -o q.o")
          "gcc print.o q.o double.o -o print"
          "./print"]

The wrinkle is actually a bit deeper than this too. Suppose
we are using other registers, maybe some that are not used
for parameters, but nonetheless are registers that the
function we're calling would like to use? Without knowing
the details of how the function is implemented, we could be
defensive and save @emph{everything} we're using with the
assumption the called function may clobber anything. But
here, the ABI comes into play again. There are conventions
around who is responsible for registers in calls. The called
function is responsible for maintaining the registers
@racket['rbx], @racket['rsp], @racket['rbp], @racket['r12],
@racket['r13], @racket['r14], @racket['r15]; these are
@bold{callee-saved registers}. This means we, the callers,
don't have to worry about these registers being clobbered
and don't need to save them to the stack. If the called
function wants to use these registers, it's responsible for
saving their value and restoring them before returning. On
the other hand, registers @racket['rax], @racket['rdi],
@racket['rdx], @racket['rcx], @racket['r8], @racket['r9],
@racket['r10], and @racket['r11] are @bold{caller-saved
 registers}, which means the called function is free to
clobber them and if we want to preserve their value across
the call, we'll need to save and restore them.

As a final note, keep in mind that the compiler generates
code this is both called and a caller, so it has to be
mindful of both sides of the convention. The main entry
point @tt{entry} is called from the C run-time. If the
generated code wants to use any of the callee-saved
registers, it should save them and restore them before the
return that delivers the final result of evaluation. On the
other hand, when it calls external functions implemented in
C, it is the caller and has to maintain the caller-saved
registers.

OK, now let's use these new powers to write the compiler.

@section{A Compiler for Evildoer}


Implementing @racket[eof], @racket[void],
@racket[eof-object?] and @racket[begin] are all
straightfoward and don't really involve anything new.

For @racket[peek-byte], @racket[read-byte], and
@racket[write-byte], we generate code that calls the
appropriate C function. In the case of @racket[write-byte],
we arrange for the byte that we'd like to write to be in
@racket['rdi] before the call.


The complete compiler:

@codeblock-include["evildoer/compile.rkt"]

We can continue to interactively try out examples with
@racket[asm-interp], although there are two issues we need
to deal with.

The first is that the @racket[asm-interp] utility doesn't
know anything about the Evildoer run-time. Hence we need to
tell @racket[asm-interp] to link in @tt{byte.o} when running
an example; otherwise labels like @tt{byte_write} will be
undefined.

The other is that we need to have an @racket[asm-interp/io]
counterpart that is analogous to @racket[interp/io], i.e. we
need to be able to redirect input and output so that we can
run programs in a functional way.

There is a parameter that @racket[asm-interp] uses called
@racket[current-objs] that can be used to add additional
object files to be linked against when running examples.

So for example, to make an example with the @tt{dbl}
function from before, we can do the following:


@;{Sneakily recompile double.o with -fPIC. Should we be more
 upfront about this stuff? I don't want to get too down in
 the weeds on how asm-interp works. (This is needed on
 elf64.)}
@;(void (ev '(system "gcc -fPIC -c double.c -o double.o")))


@ex[
 (system "gcc -fPIC -c double.c -o double.o")
 (current-objs '("double.o"))
 (define r
   (prog (Extern 'dbl)
                   (Label 'entry)
                   (Mov 'rdi 21)
                   (Call 'dbl)
                   (Ret)))
 (displayln (asm-string r))
 (eval:error (asm-interp r))
   
 #;
 (asm-interp (prog (Extern 'dbl)
                   (Label 'entry)
                   (Mov 'rdi 21)
                   (Call 'dbl)
                   (Ret)))]

The other issue is bit uglier to deal with. We need to do
this redirection at the C-level. Our solution is write an
alternative version of @tt{byte.o} that has functions for
setting the input and out streams that are used in @tt{
 write_byte} etc. The implementation of
@racket[asm-interp/io] is expected to be linked against a
library that implements these functions and will use them to
set up temporary files and redirect input and output there.
It's a hack, but a useful one.

You can see the alternative implementation of @tt{byte.c} in
@link["code/evildoer/byte-shared.c"]{@tt{byte-shared.c}} if
interested. Once compiled, it can be used with
@racket[current-objs] in order to interactively run examples
involving IO:

@;{
@ex[
 (current-objs '("byte-shared.o"))
 (asm-interp/io
   (prog (Extern 'read_byte)
         (Extern 'write_byte)
         (Label 'entry)
         (Call 'read_byte)
         (Mov 'rdi 'rax)
         (Call 'write_byte)
         (Mov 'rax 42)
         (Ret))
   "a")]
}
