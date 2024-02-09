#lang scribble/manual

@(require (for-label (except-in racket compile)
                     a86))

@(require scribble/examples
	  redex/reduction-semantics
          redex/pict
	  (only-in pict scale)
	  (only-in racket system)
	  "../fancyverb.rkt"
	  "../utils.rkt"
	  "utils.rkt"
	  "ev.rkt")

@(ev '(require rackunit a86))

@(ev `(current-directory ,(path->string (build-path notes "a86"))))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path notes "a86")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@; compile time generation of tri.s and main.c so they can be listed
@(require (for-syntax racket/base "utils.rkt"))
@(begin-for-syntax
   (require racket/system a86/ast a86/printer)
   (define (tri n)
      (list (Global 'entry)
         (Label 'entry)
         (Mov 'rbx n)      (% "the \"input\"")
         (%%% "tri: a recursive function for computing nth")
         (%%% "triangular number, where n is given in rbx.")
         (Label 'tri)
         (Cmp 'rbx 0)      (% "if rbx = 0, done")
         (Je 'done)
         (Push 'rbx)       (% "save rbx")
         (Sub 'rbx 1)
         (Call 'tri)       (% "compute tri(rbx-1) in rax")
         (Pop 'rbx)        (% "restore rbx")
         (Add 'rax 'rbx)   (% "result is rbx+tri(rbx-1)")
         (Ret)
         (Label 'done)     (% "jump here for base case")
         (Mov 'rax 0)      (% "return 0")
         (Ret)))

   (define main.c
     #<<HERE
#include <stdio.h>
#include <inttypes.h>

int64_t entry();

int main(int argc, char** argv) {
  int64_t result = entry();
  printf("%" PRId64 "\n", result);
  return 0;
}
HERE
     )

   (define gcd.c
     #<<HERE
int gcd(int n1, int n2) {
    return (n2 == 0) ? n1 : gcd(n2, n1 % n2);
}
HERE
     )

   (parameterize ([current-directory (build-path notes "a86")])
     (save-file "tri.s" (asm-string (tri 36)))
     (save-file "main.c" main.c)
     (save-file "gcd.c" gcd.c)))


@title[#:tag "a86"]{a86: a Little Assembly Language}

@emph{You need to let the little things that would ordinarily bore you suddenly thrill you.}

@table-of-contents[]

@section[#:tag "a86-Overview"]{Overview}

x86 is an instruction set architecture (ISA), which is a
fancy way of saying a programming language whose interpreter
is implemented in hardware. Really, x86 is a family of
instruction set architectures, and the first member of the
family was 8086, a 16-bit ISA for Intel's 8086 chipset.

x86 is old. It's older than the professors teaching this
class. But it lives on today in Intel and AMD based
computers in the form x86-64, the 64-bit descendant of the
8086 language.

Because it's old and because the design of each generation
of x86 has had significant backwards compatability
requirements and because modern processors are sophisticated
pieces of machinery, the x86-64 language is, well,
complicated. For example,
@link["https://software.intel.com/content/dam/develop/external/us/en/documents-tps/325462-sdm-vol-1-2abcd-3abcd.pdf"]{
 Intel's x86 software developer's manual} is 5,066 pages
long.
@link["https://www.amd.com/system/files/TechDocs/40332.pdf"]{
 AMD's manual} is 3,242 pages.


x86-64 is going to be used as the target language of our
compiler. We'll build up a translation from a very
high-level language, based on Racket, down to the very
low-level langauge of x86-64.

However, that doesn't mean we need to digest 8K+ pages of
dense technical manuals. We will only use very small
fraction of the x86-64 instruction set.

To make our lives easier, we will do what programming
language designers often do, we will abstract the behemoth
of x86-64 to a small, core language (which we call @bold{
 a86}). Our compilers will target a86 and compiling from a86
to x86 as the last step in the compiler pipeline will be
dead simple.

This chapter describes the a86 language.

@section{Giving x86 a try}

Before describing a86, let's take a brief look at x86.

There are a few different human-readable formats for writing
x86 assembly code, but we'll be using the one supported by
@link["https://www.nasm.us/"]{the Netwide Assembler} (NASM).

Here's an example x86 program, written using nasm syntax.
The program has one global label called @tt{entry}, which
will be the main entry point for the program. This program
computes the 36th triangular number, which will reside in
register @tt{rax} when the code returns.

@margin-note{The conventions for label names differ between Mac and
Linux systems.  On MacOS, you need to prefix all label names with an
underscore, while on Linux you do not.  So on a Mac, you would use the
names @tt{_entry}, @tt{_tri}, and @tt{_done}, while on Linux you would
use @tt{entry}, @tt{tri}, and @tt{done}.

This example is shown using the @(if (eq? (system-type 'os) 'macosx)
"MacOS" "Linux") naming convention, because that's what operating
system was used when this web page was built.}

@filebox-include[fancy-nasm "a86/tri.s"]

The @math{n}th triangular number is the sum of the integers
from 0 to @math{n}, so the @math{36}th triangular number is
@math{0 + 1 + 2 + 3 + ... + 34 + 35 + 36}.

This code is not intended to be a model of efficient
computation, rather it demonstrates some important x86
instructions and shows how to compute in a recursive style,
even at a low-level.

Without getting too bogged down in the details, here how the
code works. Instructions execute one after another. There
are a number of registers which can be used to hold values.
This code makes use of the @tt{rax} and @tt{rbx} register
(and some other registers are implicitly used and altered by
the @tt{call}, @tt{push}, @tt{pop} and @tt{ret}
instructions). The lines like @tt{entry:}, @tt{tri:}, and
@tt{done:} are not instructions, but labels -- they are
names for locations in the source code and can be used as
the targets of jumps and calls.

Suppose we start executing at @tt{entry}.

@itemlist[
 @item{@tt{mov rbx, 36} sets the @tt{rbx} register to 36.}

 @item{@tt{cmp rbx 0} compares the value in register @tt{
   rbx} to zero. Executing this instruction sets flags in the
  CPU, which affect subsequent ``conditional'' instructions.
  In this program, the next instruction is a conditional jump.}

 @item{@tt{je done} either jumps to the instruction
  following label @tt{done} or proceeds to the next
  instruction, based on the state of the comparison flags. The
  @tt{je} instruction jumps if the comparison was equal, so
  control jumps to done if @tt{rbx} was 0 in this program. If
  not, the next instruction is executed.}

 @item{@tt{push rbx} uses memory as a stack to save the
  value of @tt{rbx}. Under the hood this is modifying a
  register that holds a pointer to the stack memory location
  (register @tt{rsp}).}

 @item{@tt{sub rbx, 1} decrements @tt{rbx} by 1.}

 @item{@tt{call tri} performs something like a function
  call; it uses memory as a stack to save the current location
  in the code (which is where control should return to after
  the function has completed). After saving this return
  pointer, it jumps to the label @tt{tri}. There aren't really
  functions, but this uses the stack to mimic the
  call-and-return mechanism of functions.}

 @item{@tt{pop rbx} uses the stack memory to pop off the top
  element and move it into @tt{rbx}, adjusting the stack
  pointer appropriately.  This has the effect of restoring @tt{rbx}
  to the value saved earlier by the @tt{push}, i.e. before the decrement
  and any changes done in the call to @tt{tri}.}

 @item{@tt{add rax, rbx} updates @tt{rax} to hold @tt{rax}
  plus @tt{rbx}.}

 @item{@tt{ret} does a ``return,'' i.e. it pops an address
  from the stack and jumps to it. In this case, the jump
  either returns from to a previous call to @tt{tri} or to
  original caller of @tt{entry}.}

 @item{@tt{mov rax, 0} this instruction is only reached from
  the earlier conditional jump. It sets @tt{rax} to 0. This
  program computes its result in @tt{rax} so this is saying
  that when @tt{rbx} (the ``input'') is 0, then (the
  ``output'') is 0.}

 @item{@tt{ret} does a ``return,'' either to a prior call to
  @tt{tri} or the caller of @tt{entry}.}
 ]

Despite the lower-level mechanisms, this code computes in a
way similar to a non-tail recursive definition of the
@racket[tri] function written in Racket:

@racketblock[
 (define (tri n)
   (if (= n 0)
       0
       (+ n (tri (sub1 n)))))
 (tri 36)
]

@margin-note{As an exercise to check your understanding, try writing a
tail-recursive version of @racket[tri] and the corresponding
x86 code, which should not need to push anything to the
stack or use the @tt{call} instruction.}

We can compile the @tt{tri.s} assembly program to an object
file with @tt{nasm}:

@margin-note{The format argument should be @tt{macho64} on
 Mac OS and @tt{elf64} on Linux.}

@shellbox[
 (format "nasm -f ~a -o tri.o tri.s"
         (if (eq? 'macosx (system-type 'os))
             "macho64"
             "elf64"))]

To run the object file, we will need to link with a small C program
that can call the @tt{entry} label of our assembly code and then
print the result:

@filebox-include[fancy-c "a86/main.c"]

Notice that from the C program's perspective, the assembly
code defines what looks like a C function called @tt{entry}
that returns an @tt{int64_t} result.

How does this work? When the C program calls @tt{entry} it
places a return pointer on the stack and jumps to @tt{
 entry}. The fact that we decided to put the result in
register @tt{rax} was not arbitrary -- that's the register
that by convention is used for a return value. When the
assembly code executes it's final @tt{ret} instruction, it
jumps back to C with the 36th triangular number in @tt{rax},
which the C side knows is the return value. This convention
is part of a larger set of conventions known as the @bold{
 Application Binary Interface}. For a reference, see the
@secref{Texts} section of the notes.


We can compile the @tt{main.c} C program to an object file with @tt{gcc}:

@shellbox[
 "gcc -c main.c -o main.o"
 ]

Now we can make an executable by linking the two together:

@shellbox[
 "gcc main.o tri.o -o tri"
]

Finally, we can run the executable:

@shellbox[
 "./tri"
]

There, of course, is a lot more to x86-64 than what's been
shown here. If you want to dig deeper, check the references
in @secref{Texts}.  But now let's turn to a86.

@section{a86: Representing x86 Code as Data}

Here we will employ one of the great ideas in computer
science: we will represent programs as data. Rather than
toil away at the level of x86, writing programs directly in
nasm syntax, compiling, and running them, we will instead
design a data type definition for representing x86 programs
and @emph{compute} programs.

Our representation will not be complete -- this is going to
help us simplify x86 down to something manageable. We won't
cover every instruction and we won't cover every variation
of the instructions we do cover.

An a86 program is a list of a86 instructions. Each
instruction is represented as a structure, described in the
following section.

@margin-note{Before working through these examples, you'll need to
install the @racketmodname[a86] module, part of the @tt{langs} package
for this course.  See @secref{langs-package} for details on
installing.}

Here's the triangular number example:

@margin-note{@racket[%], @racket[%%], and @racket[%%%] are
 constructors for assembly comments.}

@#reader scribble/comment-reader
 (ex
 ; import the a86 library
 (require a86)
 ; a86 code that computes the 36th triangular number
 (define tri-36
   (list (Global 'entry)
         (Label 'entry)
         (Mov 'rbx 36)     (% "the \"input\"")
         (%%% "tri: a recursive function for computing nth")
         (%%% "triangular number, where n is given in rbx.")
         (Label 'tri)
         (Cmp 'rbx 0)      (% "if rbx = 0, done")
         (Je 'done)
         (Push 'rbx)       (% "save rbx")
         (Sub 'rbx 1)
         (Call 'tri)       (% "compute tri(rbx-1) in rax")
         (Pop 'rbx)        (% "restore rbx")
         (Add 'rax 'rbx)   (% "result is rbx+tri(rbx-1)")
         (Ret)
         (Label 'done)     (% "jump here for base case")
         (Mov 'rax 0)      (% "return 0")
         (Ret)))
)

This code should look familiar. At first glance it's just
the x86 code with more parentheses. @margin-note{And who
 doesn't love more parentheses?} But something fundamental
has happended. This a86 program is just a value in Racket.
This means we can use Racket as a @bold{Meta-Language} to
write programs that compute @emph{with} x86 programs.

So for example, let's say you have two a86 programs and you
want to glue them together into one: well that just
@racket[append]. Suppose you want to compute which registers
are used in a given a86 program? Suppose you want to replace
uses of @racket['rax] with @racket['rdi]? It just a matter
of writing the right Racket function to do it.

Here's another immediate benefit. Instead of writing a
single x86 program, let's write an infinite set of a86
programs, one for computing each @math{n}th triangular
number.  Easy-peasy:

@#reader scribble/comment-reader
 (ex
 ; Natural -> a86
 ; Computes a86 code that computes the @math{n}th triangular number
 (define (tri n)
   (list (Global 'entry)
         (Label 'entry)
         (Mov 'rbx n)      (% "the \"input\"")
         (%%% "tri: a recursive function for computing nth")
         (%%% "triangular number, where n is given in rbx.")
         (Label 'tri)
         (Cmp 'rbx 0)      (% "if rbx = 0, done")
         (Je 'done)
         (Push 'rbx)       (% "save rbx")
         (Sub 'rbx 1)
         (Call 'tri)       (% "compute tri(rbx-1) in rax")
         (Pop 'rbx)        (% "restore rbx")
         (Add 'rax 'rbx)   (% "result is rbx+tri(rbx-1)")
         (Ret)
         (Label 'done)
         (Mov 'rax 0)
         (Ret)))

  ; recreate original program
  (define tri-36 (tri 36))
)

It's also easy to go from our data representation to its
interpretation as an x86 program.

There is a function provided for printing an a86 program as an x86
program using nasm notation, called @racket[asm-display].  Calling
this function prints to the current output port, but it's also
possible to write the output to a file or convert it to a string.

@margin-note{The @racket[asm-display] function knows what OS you are
using and adjusts the label naming convention to use underscores or
not, so that you don't have to worry about it.}

@ex[
(asm-display (tri 36))
    ]

Notice how this generates exactly what you saw in @tt{tri.s}.

From here, we can assemble, link, and execute.

We can also, since we have a general purpose programming
language at our disposal in the meta-language, write a
program to do all that for us:

@ex[
 (asm-interp (tri 36))
 ]

The @racket[asm-interp] function consumes an @tt{a86}
program as input and produces the integer result the program
computes, i.e. it is an @bold{Interpreter} for a86. Behind
the scenes it does this by converting to nasm, assemblying,
compiling a thin C wrapper, executing the program, and
reading the result. This will be a rather handy tool both in
interactively exploring the a86 language (you can write
assembly in a REPL), but also an important tool when it
comes time to test the compilers we write.

@section[#:tag "stacks"]{Stacks: pushing, popping, calling, returning}

The a86 execution model includes access to memory that can
be used as a stack data structure. There are operations that
manipulate the stack, such as @racket[Push], @racket[Pop],
@racket[Call], and @racket[Ret], and the stack register
pointer @racket['rsp] is dedicated to the stack. Stack
memory is allocated in ``low'' address space and grows
downward. So pushing an element on to the stack @emph{
 decrements} @racket['rsp].

The stack is useful as a way to save away values that may be
needed later. For example, let's say you have two
(assembly-level) functions and you want to produce the sum
of their results. By convention, functions return their
result in @racket['rax], so doing something like this
won't work:

@racketblock[
(seq (Call 'f)
     (Call 'g)
     (Add 'rax ...))
]

The problem is the return value of @racket['f] gets
clobbered by @racket['g]. You might be tempted to fix the
problem by moving the result to another register:

@racketblock[
(seq (Call 'f)
     (Mov 'rbx 'rax)
     (Call 'g)
     (Add 'rax 'rbx))
]

This works only so long as @racket['g] doesn't clobber
@racket['rbx]. In general, it might not be possible to avoid
that situation.  So the solution is to use the stack to save
the return value of @racket['f] while the call to @racket['g]
proceeds:

@racketblock[
(seq (Call 'f)
     (Push 'rax)
     (Call 'g)
     (Pop 'rbx)
     (Add 'rax 'rbx))
]

This code pushes the value in @racket['rax] on to the stack
and then pops it off and into @racket['rbx] after
@racket['g] returns. Everything works out so long as
@racket['g] maintains a stack-discipline, i.e. the stack
should be in the same state when @racket['g] returns as when
it was called.

We can make a complete example to confirm that this works as
expected. First let's set up a little function for letting
us try out examples:

@#reader scribble/comment-reader
(ex
(define (eg asm)
  (asm-interp
   (prog
    (Global 'entry)
    (Label 'entry)
    asm  ; the example code we want to try out
    (Ret)

    (Label 'f)      ; calling 'f returns 36
    (Mov 'rax 36)
    (Ret)

    (Label 'g)      ; calling 'g returns 6, but
    (Mov 'rbx 4)    ; it clobbers 'rbx just for the lulz
    (Mov 'rax 6)
    (Ret))))
)

Now let's try it, using the stack to confirm it does the
right thing:

@#reader scribble/comment-reader
(ex
(eg (seq (Call 'f)
         (Push 'rax)
         (Call 'g)
         (Pop 'rbx)
         (Add 'rax 'rbx)))
)

Compare that with the first version that used a register to
save the result of @racket['f]:

@#reader scribble/comment-reader
(ex
(eg (seq (Call 'f)
         (Mov 'rbx 'rax)
         (Call 'g)
         (Add 'rax 'rbx)))
)


The @racket[Push] and @racket[Pop] instructions offer a
useful illusion, but of course, there's not really any data
structure abstraction here; there's just raw memory and
registers. But so long as code abides by conventions, the
illusion turns out to be the true state of affairs.

What's really going on under the hood of @racket[Push] and
@racket[Pop] is that the @racket['rsp] register is
decremented and the value is written to the memory location
pointed to by the value of @racket['rsp].

The following code is @emph{mostly} equivalent to what we wrote
above (and we will discuss the difference in the next section):

@#reader scribble/comment-reader
(ex
(eg (seq (Call 'f)
         (Sub 'rsp 8)                ; "allocate" a word on the stack
         (Mov (Offset 'rsp 0) 'rax)  ; write 'rax to top frame
         (Call 'g)
         (Mov 'rbx (Offset 'rsp 0))  ; load top frame into 'rbx
         (Add 'rsp 8)                ; "deallocate" word on the stack
         (Add 'rax 'rbx)))
)

As you can see from this code, it would be easy to violate
the usual invariants of stack data structure to, for
example, access elements beyond the top of the stack. The
value of @racket[Push] and @racket[Pop] is they make clear
that you are using things in a stack-like way and they keep
you from screwing up the accesses, offsets, and adjustments
to @racket['rsp].

Just as @racket[Push] and @racket[Pop] are useful illusions,
so too are @racket[Call] and @racket[Ret]. They give the
impression that there is a notion of a procedure and
procedure call mechanism in assembly, but actually there's
no such thing.

Think for a moment about what it means to ``call'' @racket['f]
in the examples above. When executing @racket[(Call 'f)],
control jumps to the instruction following
@racket[(Label 'f)]. When we then get to @racket[(Ret)],
somehow the CPU knows to jump @emph{back} to the instruction
following the @racket[(Call 'f)] that we started with.

What's really going on is that @racket[(Call 'f)] is pushing
the address of subsequent instruction on to the stack and
then jumping to the label @racket['f]. This works in concert
with @racket[Ret], which pops the return address off the
stack and jumping to it.

Just as we could write equivalent code without @racket[Push]
and @racket[Pop], we can write the same code without
@racket[Call] and @racket[Ret].

We do need one new trick, which is the @racket[Lea]
instruction, which loads an effective address. You can think
of it like @racket[Mov] except that it loads the address of
something rather than what is pointed to by an address.  For our
purposes, it is useful for loading the address of a label:

@racketblock[
 (Lea 'rax 'f)
 ]

This instruction puts @emph{the address} of label
@racket['f] into @racket[rax]. You can think of this as
loading a @emph{function pointer} into @racket['rax]. With
this new instruction, we can illuminate what is really going
on with @racket[Call] and @racket[Ret]:

@#reader scribble/comment-reader
(ex
(eg (seq (Lea 'rax 'fret)  ; load address of 'fret label into 'rax
         (Push 'rax)       ; push the return pointer on to stack
         (Jmp 'f)          ; jump to 'f
         (Label 'fret)     ; <-- return point for "call" to 'f
         (Push 'rax)       ; save result (like before)
         (Lea 'rax 'gret)  ; load address of 'gret label into 'rax
         (Push 'rax)       ; push the return pointer on to stack
         (Jmp 'g)          ; jump to 'g
         (Label 'gret)     ; <-- return point for "call" to 'g
         (Pop 'rbx)        ; pop saved result from calling 'f
         (Add 'rax 'rbx)))
)

@;{
Or to avoid the use of register to temporarily hold the
address to jump to, we could've also written it as:

@#reader scribble/comment-reader
(ex
(eg (seq (Sub 'rsp 8)      ; allocate a frame on the stack
                           ; load address of 'fret label into top of stack
         (Lea (Offset 'rsp 0) 'fret)
         (Jmp 'f)          ; jump to 'f
         (Label 'fret)     ; <-- return point for "call" to 'f
         (Push 'rax)       ; save result (like before)
         (Sub 'rsp 8)      ; allocate a frame on the stack
                           ; load address of 'gret label into top of stack
         (Lea (Offset 'rsp 0) 'gret)
         (Jmp 'g)          ; jump to 'g
         (Label 'gret)     ; <-- return point for "call" to 'g
         (Pop 'rbx)        ; pop saved result from calling 'f
         (Add 'rax 'rbx)))
)
}

The above shows how to encode @racket[Call] as @racket[Lea],
@racket[Push], and @racket[Jmp].  The encoding of @racket[Ret] is just:

@racketblock[
 (seq (Pop 'rbx)    ; pop the return pointer
      (Jmp 'rbx))   ; jump to it
 ]



@section[#:tag "a86-flags"]{Flags}

As mentioned earlier, the processor makes use of @emph{flags} to
handle comparisons. For our purposes, there are four flags to
be aware of: zero (ZF), sign (SF), carry (CF), and overflow (OF).

These flags are set by each of the arithmetic operations, which
are appropriately annotated in the @secref{a86-instructions}.
Each of these operations is binary (meaning they take two
arguments), and the flags are set according to properties of
the result of the arithmetic operation. Many of these properties
look at the most-significant bit (MSB) of the inputs and output.

@itemlist[
 @item{@bold{ZF} is set when the result is @tt{0}.}
 @item{@bold{SF} is set when the MSB of the result is set.}
 @item{@bold{CF} is set when a bit was set beyond the MSB.}
 @item{@bold{OF} is set when one of two conditions is met:

   @itemlist[#:style 'ordered
    @item{The MSB of each input is @emph{set} and the MSB of
          the result is @emph{not set}.}
    @item{The MSB of each input is @emph{not set} and the MSB
          of the result is @emph{set}.}
   ]}
]

Note that CF is only useful for unsigned arithmetic, while OF
is only useful for signed arithmetic. In opposite cases, they
provide no interesting information.

These flags, along with many others, are stored in a special
FLAGS register that cannot be accessed by normal means. Each
flag is represented by a single bit in the register, and they
all have specific bits assigned by the x86 specification. For
example, CF is bit 0, ZF is bit 6, SF is bit 7, and OF is bit
11, as indexed from the least-significant bit position (but
you don't need to know these numbers).

The various conditions that can be tested for correspond to
combinations of the flags. For example, the @racket[Jc]
instruction will jump if CF is set, otherwise execution will
fall through to the next instruction. Most of the condition
suffixes are straightforward to deduce from their spelling,
but some are not. The suffixes (e.g., the @tt{c} in @tt{Jc})
and their meanings are given below. For brevity's sake the
flags' names are abbreviated by ommitting the F suffix and
prefixing them with either @tt{+} or @tt{-} to indicate set
and unset positions, respectively, as needed. Some of the
meanings require use of the bitwise operators @tt{|} (OR),
@tt{&} (AND), @tt{^} (XOR), and @tt{=?} (equality).

@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
  (list (list @bold{Suffix} @bold{Flag}      @bold{Suffix} @bold{Flag})
        (list @tt{z}        @tt{+Z}          @tt{nz}       @tt{-Z})
        (list @tt{e}        @tt{+Z}          @tt{ne}       @tt{-Z})
        (list @tt{s}        @tt{+S}          @tt{ns}       @tt{-S})
        (list @tt{c}        @tt{+C}          @tt{nc}       @tt{-C})
        (list @tt{o}        @tt{+O}          @tt{no}       @tt{-O})
        (list @tt{l}        @tt{      (S ^ O)}    @tt{g}        @tt{(-Z & (S =? O))})
        (list @tt{le}       @tt{(+Z | (S ^ O))}  @tt{ge}       @tt{      (S =? O)}))]

The @tt{e} suffix (``equal?'') is just a synonym
for the @tt{z} suffix (``zero?''). This is because it is
common to use the @racket[Cmp] instruction to perform
comparisons, but @racket[Cmp] is actually identical to
@racket[Sub] with the exception that the result is not
stored anywhere (i.e., it is only used for setting flags
according to subtraction). If two values are subtracted
and the resulting difference is zero (ZF is set), then the
values are equal.


@subsection{Push and Pop}

In the previous section (@secref{stacks}), it was explained
that the @racket[Push] and @racket[Pop] operations are
essentially equivalent to manually adjusting the stack
pointer and target register. The one difference is that these
special stack-manipulation operations do not set any flags
like @racket[Add] and @racket[Sub] do. So while you can
often choose to manually implement stack manipulation, you'll
need to use these instructions specifically if you want to
preserve the condition flags after adjusting the stack.




@section{a86 Reference}

@defmodule[a86 #:no-declare]

@margin-note{The a86 language may evolve some over the
 course of the semester, but we will aim to document any
 changes by updating this section. Also, because the run-time
 system changes for each language, you may need to do some
 work to have @racket[asm-interp] cooperate with your
 run-time system.}

This module provides all of the bindings from
@racketmodname[a86/ast], @racketmodname[a86/printer],
and @racketmodname[a86/interp], described below.

@section[#:tag "a86-instructions"]{Instruction set}

@defmodule[a86/ast]

This section describes the instruction set of a86.

There are 16 registers: @racket['rax], @racket['rbx], @racket['rcx],
@racket['rdx], @racket['rbp], @racket['rsp], @racket['rsi],
@racket['rdi], @racket['r8], @racket['r9], @racket['r10],
@racket['r11], @racket['r12], @racket['r13], @racket['r14], and
@racket['r15]. These registers are 64-bits wide.  There is also
@racket['eax] which accesses the lower 32-bits of @racket['rax].
This is useful in case you need to read or write 32-bits of memory.

The registers @racket['rbx], @racket['rsp], @racket['rbp], and
@racket['r12] through @racket['r15] are ``callee-saved'' registers,
meaning they are preserved across function calls (and must be saved
and restored by any callee code).

Each register plays the same role as in x86, so for example
@racket['rsp] holds the current location of the stack.

@defproc[(register? [x any/c]) boolean?]{
 A predicate for registers.
}

@defproc[(label? [x any/c]) boolean?]{
 A predicate for label @emph{names}, i.e. symbols which are not register names.

 Labels must also follow the NASM restrictions on label names: "Valid
 characters in labels are letters, numbers, @tt{_}, @tt{$}, @tt{#}, @tt{@"@"}, @tt{~}, @tt{.}, and
 @tt{?}. The only characters which may be used as the first character of an
 identifier are letters, @tt{.} (with special meaning), @tt{_}
 and @tt{?}."

 @ex[
 (label? 'foo)
 (label? "foo")
 (label? 'rax)
 (label? 'foo-bar)
 (label? 'foo.bar)
 ]

}

@defproc[(instruction? [x any/c]) boolean?]{
 A predicate for instructions.
}

@defproc[(offset? [x any/c]) boolean?]{
 A predicate for offsets.
}

@defproc[(64-bit-integer? [x any/c]) boolean?]{
 A predicate for determining if a value is an integer that fits in 64-bits.

 @ex[
 (64-bit-integer? 0)
 (64-bit-integer? (sub1 (expt 2 64)))
 (64-bit-integer? (expt 2 64))
 (64-bit-integer? (- (expt 2 63)))
 (64-bit-integer? (sub1 (- (expt 2 63))))]
}

@defproc[(32-bit-integer? [x any/c]) boolean?]{
 A predicate for determining if a value is an integer that fits in 64-bits.

 @ex[
 (32-bit-integer? 0)
 (32-bit-integer? (sub1 (expt 2 32)))
 (32-bit-integer? (expt 2 32))
 (32-bit-integer? (- (expt 2 32)))
 (32-bit-integer? (sub1 (- (expt 2 32))))]
}

@defproc[(seq [x (or/c instruction? (listof instruction?))] ...) (listof instruction?)]{
 A convenience function for splicing togeter instructions and lists of instructions.

  @ex[
 (seq)
 (seq (Label 'foo))
 (seq (list (Label 'foo)))
 (seq (list (Label 'foo)
            (Mov 'rax 0))
      (Mov 'rdx 'rax)
      (list (Call 'bar)
            (Ret)))
 ]
}

@defproc[(prog [x (or/c instruction? (listof instruction?))] ...) (listof instruction?)]{

 Like @racket[seq], but also checks that the instructions
 are well-formed in the following sense:

 @itemlist[

 @item{Programs have at least one label which is declared @racket[Global]; the first label is used as the entry point.}
 @item{All label declarations are unique.}
 @item{All label targets are declared.}
 @item{... other properties may be added in the future.}

 ]

 This function is useful to do some early error checking
 over whole programs and can help avoid confusing NASM
 errors. Unlike @racket[seq] it should be called at the
 outermost level of a function that produces a86 code and not
 nested.

 @ex[
 (prog (Global 'foo) (Label 'foo))
 (eval:error (prog (Label 'foo)))
 (eval:error (prog (list (Label 'foo))))
 (eval:error (prog (Mov 'rax 32)))
 (eval:error (prog (Label 'foo)
                   (Label 'foo)))
 (eval:error (prog (Jmp 'foo)))
 (prog (Global 'foo)
       (Label 'foo)
       (Jmp 'foo))
 ]
}

@defproc[(symbol->label [s symbol?]) label?]{

  Returns a modified form of a symbol that follows NASM label conventions.

  @ex[
  (let ([l (symbol->label 'my-great-label)])
    (seq (Label l)
         (Jmp l)))
  ]
}

@deftogether[(@defstruct*[% ([s string?])]
               @defstruct*[%% ([s string?])]
               @defstruct*[%%% ([s string?])])]{

 Creates a comment in the assembly code. The @racket[%]
 constructor adds a comment toward the right side of the
 current line; @racket[%%] creates a comment on its own line
 1 tab over; @racket[%%%] creates a comment on its own line
 aligned to the left.

 @#reader scribble/comment-reader
 (ex
 (asm-display
   (prog (Global 'foo)
         (%%% "Start of foo")
         (Label 'foo)
         ; Racket comments won't appear
         (%% "Inputs one argument in rdi")
         (Mov 'rax 'rdi)
         (Add 'rax 'rax)    (% "double it")
         (Sub 'rax 1)       (% "subtract one")
         (%% "we're done!")
         (Ret))))
}

@defstruct*[Offset ([r register?] [i exact-integer?])]{

 Creates an memory offset from a register. Offsets are used
 as arguments to instructions to indicate memory locations.
 An error is signalled when given invalid inputs.

 @ex[
 (Offset 'rax 0)
 (eval:error (Offset 'rax 4.1))
 ]
}

@defstruct*[Text ()]{

 Declares the start of a text section, which includes instructions to
 be executed.

}

@defstruct*[Data ()]{

 Declares the start of a data section, which includes data and constants.

}

@defstruct*[Label ([x label?])]{

 Creates a label from the given symbol. Each label in a
 program must be unique.  Register names cannot be used
 as label names and names must follow the NASM restrictions
 on valid label names (see @racket[label?] for details).

 @ex[
 (Label 'fred)
 (eval:error (Label "fred"))
 (eval:error (Label 'rax))
 (eval:error (Label 'fred-wilma))
 ]

}

@defstruct*[Extern ([x label?])]{

 Declares an external label.

}

@defstruct*[Global ([x label?])]{

 Declares a label as global, i.e. linkable with other object files.

}


@defstruct*[Call  ([x (or/c label? register?)])]{

 A call instruction.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Call 'f)
   (Add 'rax 1)
   (Ret)
   (Label 'f)
   (Mov 'rax 41)
   (Ret)))
 ]
}

@defstruct*[Ret ()]{

 A return instruction.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Ret)))
 ]

}

@defstruct*[Mov ([dst (or/c register? offset?)] [src (or/c register? offset? 64-bit-integer?)])]{

 A move instruction. Moves @racket[src] to @racket[dst].

 Either @racket[dst] or @racket[src] may be offsets, but not both.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rbx 42)
   (Mov 'rax 'rbx)
   (Ret)))
 (eval:error (Mov (Offset 'rax 0) (Offset 'rbx 0)))
 ]

}

@defstruct*[Add ([dst register?] [src (or/c register? offset? 32-bit-integer?)])]{

 An addition instruction. Adds @racket[src] to @racket[dst]
 and writes the result to @racket[dst]. Updates the conditional flags.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 32)
   (Add 'rax 10)
   (Ret)))
 ]
}

@defstruct*[Sub ([dst register?] [src (or/c register? offset? 32-bit-integer?)])]{

 A subtraction instruction. Subtracts @racket[src] from
 @racket[dst] and writes the result to @racket[dst].
 Updates the conditional flags.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 32)
   (Sub 'rax 10)
   (Ret)))
 ]
}

@defstruct*[Cmp ([a1 (or/c register? offset?)] [a2 (or/c register? offset? 32-bit-integer?)])]{
 Compare @racket[a1] to @racket[a2] by subtracting @racket[a2] from @racket[a1]
 and updating the comparison flags. Does not store the result of subtraction.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 2)
   (Jg 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jmp ([x (or/c label? register?)])]{
 Jump to label @racket[x].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Jmp 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Pop 'rbx)
   (Jmp 'rbx)))
 ]

}

@defstruct*[Jz ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the zero flag is set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 2)
   (Jz 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jnz ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the zero flag is @emph{not} set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 2)
   (Jnz 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Je ([x (or/c label? register?)])]{
 An alias for @racket[Jz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 2)
   (Je 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jne ([x (or/c label? register?)])]{
 An alias for @racket[Jnz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 2)
   (Jne 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jl ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the conditional flags are set to ``less than'' (see @secref{a86-flags}).

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 2)
   (Jl 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jle ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the conditional flags are set to ``less than or equal'' (see @secref{a86-flags}).

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 42)
   (Jle 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jg ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the conditional flags are set to ``greater than'' (see @secref{a86-flags}).

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 2)
   (Jg 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jge ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the conditional flags are set to ``greater than or equal'' (see @secref{a86-flags}).

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 42)
   (Jg 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jo ([x (or/c label? register?)])]{
 Jump to @racket[x] if the overflow flag is set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (sub1 (expt 2 63)))
   (Add 'rax 1)
   (Jo 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jno ([x (or/c label? register?)])]{
 Jump to @racket[x] if the overflow flag is @emph{not} set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (sub1 (expt 2 63)))
   (Add 'rax 1)
   (Jno 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jc ([x (or/c label? register?)])]{
 Jump to @racket[x] if the carry flag is set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax -1)
   (Add 'rax 1)
   (Jc 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jnc ([x (or/c label? register?)])]{
 Jump to @racket[x] if the carry flag is @emph{not} set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax -1)
   (Add 'rax 1)
   (Jnc 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Cmovz ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the zero flag is set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovz 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 2)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovz 'rax 'r9)
   (Ret)))
 ]
}


@defstruct*[Cmove ([dst register?] [src (or/c register? offset?)])]{
 An alias for @racket[Cmovz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmove 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 2)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmove 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovnz ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the zero flag is @emph{not} set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovnz 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 2)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovnz 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovne ([dst register?] [src (or/c register? offset?)])]{
 An alias for @racket[Cmovnz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovne 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 2)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovne 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovl ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the conditional flags are set to ``less than'' (see @secref{a86-flags}).

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovl 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax -1)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovl 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovle ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the conditional flags are set to ``less than or equal'' (see @secref{a86-flags}).

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovle 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 2)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovle 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovg ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the conditional flags are set to ``greather than'' (see @secref{a86-flags}).

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovg 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 2)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovg 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovge ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the conditional flags are set to ``greater than or equal'' (see @secref{a86-flags}).

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax -1)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovge 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 2)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovge 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovo ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the overflow flag is set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 63) 1))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovo 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 63) 2))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovo 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovno ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the overflow flag is @emph{not} set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 63) 1))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovno 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 63) 2))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovno 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovc ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the carry flag is set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 64) 1))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovc 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 64) 2))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovc 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovnc ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the carry flag is @emph{not} set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 64) 1))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovnc 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 64) 2))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovnc 'rax 'r9)
   (Ret)))
 ]
}


@defstruct*[And ([dst (or/c register? offset?)] [src (or/c register? offset? 32-bit-integer?)])]{

 Compute logical ``and'' of @racket[dst] and @racket[src] and put result in @racket[dst]. Updates the conditional flags.

 @#reader scribble/comment-reader
 (ex
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b1011) ; #b1011 = 11
   (And 'rax #b1110) ; #b1110 = 14
   (Ret)))           ; #b1010 = 10
 )
}

@defstruct*[Or ([dst (or/c register? offset?)] [src (or/c register? offset? 32-bit-integer?)])]{
 Compute logical ``or'' of @racket[dst] and @racket[src] and put result in @racket[dst]. Updates the conditional flags.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @#reader scribble/comment-reader
 (ex
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b1011) ; #b1011 = 11
   (Or 'rax #b1110)  ; #b1110 = 14
   (Ret)))           ; #b1111 = 15
 )
}

@defstruct*[Xor ([dst (or/c register? offset?)] [src (or/c register? offset? 32-bit-integer?)])]{
 Compute logical ``exclusive or'' of @racket[dst] and @racket[src] and put result in @racket[dst]. Updates the conditional flags.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @#reader scribble/comment-reader
 (ex
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b1011) ; #b1011 = 11
   (Xor 'rax #b1110) ; #b1110 = 14
   (Ret)))           ; #b0101 = 5
 )
}

@defstruct*[Sal ([dst register?] [i (integer-in 0 63)])]{
 Shift @racket[dst] to the left @racket[i] bits and put result in @racket[dst].
 The leftmost bits are discarded. Updates the conditional flags.

 @#reader scribble/comment-reader
 (ex
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b100) ; #b100 = 4 = 2^2
   (Sal 'rax 6)
   (Ret)))          ; #b100000000 = 256
 )
}

@defstruct*[Sar ([dst register?] [i (integer-in 0 63)])]{
 Shift @racket[dst] to the right @racket[i] bits and put result in @racket[dst].
 The rightmost bits are discarded.  The added leftmost bits are filled with the
 sign bit of the original. Updates the conditional flags.

 @#reader scribble/comment-reader
 (ex
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b100000000) ; #b100000000 = 256
   (Sar 'rax 6)
   (Ret)))        ; #b100 = 4

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b100001101) ; #b100001101 = 269
   (Sar 'rax 6)
   (Ret)))        ; #b100 = 4
 )
}

@defstruct*[Push ([a1 (or/c 32-bit-integer? register?)])]{

 Decrements the stack pointer and then stores the source
 operand on the top of the stack.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Push 'rax)
   (Mov 'rax 0)
   (Pop 'rax)
   (Ret)))
 ]
}

@defstruct*[Pop ([a1 register?])]{
 Loads the value from the top of the stack to the destination operand and then increments the stack pointer.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Push 'rax)
   (Mov 'rax 0)
   (Pop 'rax)
   (Ret)))
 ]
}

@defstruct*[Not ([a1 register?])]{
Perform bitwise not operation (each 1 is set to 0, and each 0 is set to 1) on the destination operand.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Not 'rax)
   (Ret)))
 ]
}

@defstruct*[Lea ([dst (or/c register? offset?)] [x label?])]{
 Loads the address of the given label into @racket[dst].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Lea 'rbx 'done)
   (Mov 'rax 42)
   (Jmp 'rbx)
   (Mov 'rax 0)
   (Label 'done)
   (Ret)))
 ]
}

@defstruct*[Db ([d integer?])]{
 Psuedo-instruction for declaring 8-bits of initialized static memory.
}

@defstruct*[Dw ([d integer?])]{
 Psuedo-instruction for declaring 16-bits of initialized static memory.
}

@defstruct*[Dd ([d integer?])]{
 Psuedo-instruction for declaring 32-bits of initialized static memory.
}

@defstruct*[Dq ([d integer?])]{
 Psuedo-instruction for declaring 64-bits of initialized static memory.
}

@section{From a86 to x86}

@defmodule[a86/printer]

@defproc[(asm-display [is (listof instruction?)]) void?]{

 Prints an a86 program to the current output port in nasm syntax.

 @ex[
 (asm-display (prog (Global 'entry)
                    (Label 'entry)
                    (Mov 'rax 42)
                    (Ret)))
 ]

}

@defproc[(asm-string [is (listof instruction?)]) string?]{

 Converts an a86 program to a string in nasm syntax.

 @ex[
 (asm-string (prog (Global 'entry)
                   (Label 'entry)
                   (Mov 'rax 42)
                   (Ret)))
 ]

}

@section{An Interpreter for a86}

@defmodule[a86/interp]

As you've seen throughout this chapter, @racketmodname[a86]
is equiped with an interpreter, which enables you to run
assembly programs from within Racket. This won't be directly
useful in building a compiler, but it will be very handy for
interactively exploring assembly programs and making examples
and test cases for your compiler.

The simplest form of interpreting an a86 program is to use
@racket[asm-interp].

@defproc[(asm-interp [is (listof instruction?)]) integer?]{

 Assemble, link, and execute an a86 program.

 @ex[
 (asm-interp (prog (Global 'entry)
                   (Label 'entry)
                   (Mov 'rax 42)
                   (Ret)))
 ]

 Programs do not have to start with @racket['entry]. The
 interpreter will jump to whatever the first label in the
 program is:

@ex[
 (asm-interp (prog (Global 'f)
                   (Label 'f)
                   (Mov 'rax 42)
                   (Ret)))
 ]

 The argument of @racket[asm-interp] should be a complete,
 well-formed a86 program. For best results, always use
 @racket[prog] to construct the program so that error
 checking is done early. If you use @racket[prog] and
 @racket[asm-interp] and you get a NASM syntax error message,
 please report it to the course staff as this is a bug in the
 interpreter.

 While we try to make syntax errors impossible, it is
 possible---quite easy, in fact---to write well-formed, but
 erroneous assembly programs. For example, this program tries
 to jump to null, which causes a segmentation fault:

 @ex[
 (eval:error (asm-interp (prog (Global 'crash)
                               (Label 'crash)
                               (Mov 'rax 0)
                               (Jmp 'rax))))
 ]

}

It is often the case that we want our assembly programs to
interact with the oustide or to use functionality
implemented in other programming languages. For that reason,
it is possible to link in object files to the running of an
a86 program.

The mechanism for controlling which objects should be linked
in is a parameter called @racket[current-objs], which
contains a list of paths to object files which are linked to
the assembly code when it is interpreted.

@defparam[current-objs objs (listof path-string?) #:value '()]{

Parameter that controls object files that will be linked in to
assembly code when running @racket[asm-interp].

}

For example, let's implement a GCD function in C:

@filebox-include[fancy-c "a86/gcd.c"]

First, compile the program to an object file:

@shellbox["gcc -fPIC -c gcd.c -o gcd.o"]

The option @tt{-fPIC} is important; it causes the C compiler
to emit ``position independent code,'' which is what enables
Racket to dynamically load and run the code.

Once the object file exists, using the @racket[current-objs]
parameter, we can run code that uses things defined in the C
code:

@ex[
(parameterize ((current-objs '("gcd.o")))
  (asm-interp (prog (Extern 'gcd)
                    (Global 'f)
                    (Label 'f)
                    (Mov 'rdi 11571)
                    (Mov 'rsi 1767)
                    (Sub 'rsp 8)
                    (Call 'gcd)
                    (Add 'rsp 8)
                    (Ret))))]

This will be particularly relevant for writing a compiler
where emitted code will make use of functionality defined in
a runtime system.

Note that if you forget to set @racket[current-objs], you will get a
linking error saying a symbol is undefined:

@ex[
(eval:error
  (asm-interp (prog (Extern 'gcd)
                    (Global 'f)
                    (Label 'f)
                    (Mov 'rdi 11571)
                    (Mov 'rsi 1767)
                    (Sub 'rsp 8)
                    (Call 'gcd)
                    (Add 'rsp 8)
                    (Ret))))]


@defproc[(asm-interp/io [is (listof instruction?)] [in string?]) (cons integer? string?)]{

 Like @racket[asm-interp], but uses @racket[in] for input and produce the result along
 with any output as a string.

}
