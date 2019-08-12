#lang scribble/manual
@title[#:style 'unnumbered]{Week 1: Introduction}

@table-of-contents[]

@section{What @emph{is} a Compiler?}

A function that maps an @emph{input} string to an @emph{output}
string.

@verbatim{
compiler : String -> String
}

Typically, the @emph{input} and @emph{output} strings are “programs”

@verbatim{
compiler : SourceProgram -> TargetProgram
}

For example, here are some well known compilers:

@verbatim{
gcc, clang :: C          -> Binary          -- a.out, .exe
ghc        :: Haskell    -> Binary                 
javac      :: Java       -> JvmByteCode     -- .class
scalac     :: Scala      -> JvmByteCode      
ocamlc     :: Ocaml      -> OcamlByteCode   -- .cmo
ocamlopt   :: Ocaml      -> Binary               
gwt        :: Java       -> JavaScript      -- .js
v8         :: JavaScript -> Binary
nasm       :: X86        -> Binary    
pdftex     :: LaTeX      -> PDF
pandoc     :: Markdown   -> PDF | Html | Doc
}

Key Requirements on output program:

@itemlist[#:style 'ordered
@item{Has the same @emph{meaning} (“semantics”) as input,}
@item{Is @emph{executable} in relevant @emph{context} (VM,
microprocessor, web browser).}  
]

@subsection{A Bit of History}

Compilers were invented to @link["http://worrydream.com/dbx/"]{avoid writing machine code by hand}.

@image{img/binary-soap-fortran.png}
From Binary to FORTRAN

Richard Hamming – The Art of Doing Science and Engineering, p25:

@para{@italic{In the beginning we programmed in absolute binary… Finally, a
Symbolic Assembly Program was devised – after more years than you are
apt to believe during which most programmers continued their heroic
absolute binary programming. At the time [the assembler] first
appeared I would guess about 1% of the older programmers were
interested in it – using [assembly] was “sissy stuff”, and a real
programmer would not stoop to wasting machine capacity to do the
assembly.}}

John A.N. Lee, Dept of Computer Science, Virginia Polytechnical Institute

@para{@italic{One of von Neumann’s students at Princeton recalled that
graduate students were being used to hand assemble programs into
binary for their early machine. This student took time out to build an
assembler, but when von Neumann found out about it he was very angry,
saying that it was a waste of a valuable scientific computing
instrument to use it to do clerical work.}}

@subsection{What does a Compiler @emph{look} like?}

@image{img/compiler-pipeline.png}
Compiler Pipeline

An input source program is converted to an executable binary in many stages:

@itemlist[
@item{@bold{Parsed} into a data structure called an @bold{Abstract Syntax Tree}}
@item{@bold{Checked} to make sure code is well-formed (and well-typed)}
@item{@bold{Simplified} into some convenient @bold{Intermediate Representation}}
@item{@bold{Optimized} into (equivalent) but faster program}
@item{@bold{Generated} into assembly x86}
@item{@bold{Linked} against a run-time (usually written in C)}
]

@subsection{What is CMSC 430?}

@itemlist[
@item{A bridge between two worlds
@itemlist[
@item{@emph{High-level programming languages}: OCaml (CMSC 330)}
@item{@emph{Machine code}: x86/ARM (CMSC 216)}
]}
]

A sequel to both those classes.

@itemlist[
@item{How to write @bold{a compiler} for @tt{NanoML -> X86}
@itemlist[#:style 'ordered
@item{Parsing}
@item{Checking & Validation}
@item{Simplification & Normalizing}
@item{Optimization}
@item{Code Generation}
]}
@item{But also, how to write @bold{complex programs}
@itemlist[#:style 'ordered
@item{Design}
@item{Implement}
@item{Test}
@item{@bold{Iterate}}
]
}
]

@subsection{How to write a Compiler?}

General recipe, applies to any large system

@itemlist[
@item{@emph{gradually, one feature at a time!}}
]

We will

@itemlist[
@item{Step 1 Start with a teeny tiny language,}
@item{Step 2 Build a full compiler for it,}
@item{Step 3 Add a few features,}
@item{Go to Step 2.}
]

(Yes, loops forever, but we will hit Ctrl-C in 15 weeks...)

@subsection{Mechanics}

See @secref{Syllabus}.

@subsection{What will @emph{we} do?}

Write @emph{a compiler} for @tt{NanoML -> X86}

But Rome wasn’t built in a day … and neither is any serious software.

@image{img/Eiffel.jpg}
Rome wasn’t built in a day

So we will write @emph{many} compilers:

@itemlist[
@item{Numbers and increment/decrement}
@item{Local Variables}
@item{Nested Binary Operations}
@item{Booleans, Branches and Dynamic Types}
@item{Functions}
@item{Tuples and Structures}
@item{Lambdas and closures}
@item{Types and Inference}
@item{Garbage Collection}
]

@subsection{What will @emph{you} learn?}

@bold{Core principles of compiler construction}

@itemlist[
@item{Managing Stacks & Heap}
@item{Type Checking}
@item{Intermediate forms}
@item{Optimization}
]

@bold{Several new languages}

@itemlist[
@item{Racket to write the compiler}
@item{C to write the “run-time”}
@item{x86 compilation target}
]

@bold{@italic{More importantly} how to write a large program}

@itemlist[
@item{How to use types for design}
@item{How to add new features / refactor}
@item{How to test & validate}
]

@subsection{What do you @emph{need to know}?}

This 430 depends very heavily on CMSC 330.

@itemlist[
@item{Familiarity with Functional Programming and Ocaml}
@item{Datatypes (e.g. Lists, Trees, ADTs)}
@item{Polymorphism}
@item{Recursion}
@item{Higher-order functions (e.g. map, filter, fold)}
]

Also @bold{depends on} CMSC 216

@itemlist[
@item{Experience with some C programming}
@item{Experience with some assembly (x86)}
]

@subsection{A few words on the medium of instruction}

We will use @link["https://racket-lang.org/"]{Racket} which, for our
purposes is like Ocaml but with nicer syntax.

Racket has many advanced features beyond what you saw in 330, but we
won't be using them; in the few cases we do, I’ll explain them as we
go.

@section{Our metalanguage: Racket}

@section{Our target language: x86}

@section{Our object language(s): Core Racket}

@include-section{1/ocaml-to-racket.scrbl}

@section{Compiler 0}


