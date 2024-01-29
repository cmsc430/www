#lang scribble/manual
@(require scribble/core racket/list)
@(require (for-label racket))
@(require redex/reduction-semantics
          redex/pict (only-in pict scale))

@(require scribble/examples racket/sandbox)

@(require "defns.rkt")

@(define core-racket
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator 'racket/base)))

@(core-racket '(require racket/match))

@(define-syntax-rule (ex e ...) (examples #:eval core-racket #:label #f e ...))



@(define-syntax-rule (render-grammar L)
   (scale (render-language L) 1))

@(define-syntax-rule (render-grammar/nts L nts)
   (scale (render-language L #:nts nts) 1))



@title[#:style 'unnumbered]{Software}

This course will make use of the following software:

@itemlist[

 @item{Operating system: an x86-64 ABI conforming OS such as many
  variants of Linux and macOS running on an x86-64 CPU.  It is
  also possible to use macOS on an Apple Silicon CPU with some extra
  steps.  For @secref{Windows}, see notes below.

  All students have access to the campus
  @link["http://www.grace.umd.edu/"]{GRACE} cluster, which use Red Hat
  Linux on an x86-64 CPU, an appropriate OS for this class.  See the
  @secref{GRACE} notes below.}

 @item{Racket: the implementation language and source
  language of our compilers.}

 @item{Racket @tt{langs} package: a package containing utilities
  for this course.}

 @item{NASM: the Netwide Assembler, which we will use to
  assemble x86 programs.}

 @item{GCC: the GNU compiler collection or a GCC-compatible
  system such as clang.}
]

Instruction for using each system are below:

@itemlist[
@item{@secref{GRACE}}
@item{@secref{Linux}}
@item{@secref{mac}}
@item{@secref{Windows}}
]

@section[#:tag "GRACE"]{Using GRACE}

The @link["http://www.grace.umd.edu/"]{GRACE} system gives students
access to an x86-64 Linux system that meets all of the system
requirements for the software in this course.  If you have an
incompatible system, or if you'd rather avoid installing and setting
up the software for this course, you can use GRACE.

Before using GRACE, you should locally install an implementation of
the X.Org X Window System which will enable you to run GUI programs
from GRACE on your computer (or any other computer that uses X11).  On
Linux, this is likely set up by default.  On Mac, you will need to
install @link["https://www.xquartz.org/"]{XQuartz}.  On Windows, you
can use @link["https://mobaxterm.mobatek.net/"]{MobaXterm}.

To use GRACE, open a terminal on your computer and
type:

@verbatim|{   ssh -Y <directoryID>@grace.umd.edu}|

You will prompted for your UMD Directory ID password.  After entering
your password, you will be at the GRACE command line prompt.

The @tt{-Y} command line option sets up X11 forwarding, which lets you
run GUI applications from GRACE.  If you leave this off, programs like
DrRacket will fail to launch when started.

Racket and @tt{nasm} are already installed, but you will
need to modify your @tt{PATH} environment variable so that you can
execute them from the command-line.  You can do this with the
following commands:

@verbatim|{
   # CMSC 430 set up
   set path = ( /cell_root/software/racket/8.4/sys/bin $path )
   set path = ( /cell_root/software/nasm/2.15.05/sys/bin/ $path )}|

If you add these lines to the @tt{.path} file in your home directory, then you
won't have to run this command manually every time you login; it will happen
automatically.

Once set, you should be able to run commands such as @tt{racket},
@tt{raco}, and @tt{nasm}.  Other tools such as @tt{gcc} are already
available.

Finally, you will need to install @secref{langs-package}.

@section[#:tag "Linux"]{Using Linux}

If you have an ARM-based machine, you will need to use
@seclink["GRACE"]{GRACE} or potentially setup an x86 VM.

For x86-based Linux machines, you will need to
@seclink["install-racket"]{install Racket} and the
@seclink["langs-package"]{langs package}.  Finally, install @tt{nasm}.
You can use your favorite package manager; they should all have
@tt{nasm}.


@section[#:tag "mac"]{Using macOS}

If you are using a macOS computer, the setup will be different
depending on whether you have an Intel-based CPU or an Apple Silicon
CPU.  If you're unsure which you have, click the Apple icon in the
top-left and select "About This Mac".  Under CPU, you will see
a chip name containing either Intel or Apple.

@subsection[#:tag "intel-mac"]{Using macOS on Intel}

Intel-based Macs are fairly straightforward to set up.  You will need
to @seclink["install-racket"]{install Racket} and the
@seclink["langs-package"]{langs package}.  You will also need to
install @tt{nasm}.  It's probably easiest to use a package manager
such as @link["https://brew.sh/"]{Homebrew} to install with @tt{brew
install nasm}.

You will also want to make sure your Racket installation is visible
from your @tt{PATH} environment variable.  Assuming Racket was
installed in the usual location, you can run:

@verbatim|{   export PATH=$PATH:"/Applications/Racket v|@|racket-version|/bin"}|

NOTE: You'll need to know what version of Racket you installed and use that
version's name in the above command. For example, if you install Racket 8.6,
you should be using the path @tt{"/Applications/Racket 8.6/bin"} instead.

You can add this line to the @tt{.zshrc} file in your home directory so that it
is available every time you start the Terminal. Note that once you make this
change to the @tt{.zshrc}, you'll either need to restart your terminal
application or run @tt{source ~/.zshrc} to update your current @tt{PATH}
settings.

@subsection[#:tag "apple-silicon-mac"]{Using macOS on Apple Silicon}

It's also possible to run everything we need on an Apple Silicon Mac
even though it doesn't use an x86 CPU and instead uses an ARM
processor. That's because Apple provides a compability layer called Rosetta
that will allow you run x86 programs on your ARM CPU.

The set up is basically the same as when @secref{intel-mac}, except
that when you install Racket you need to select the installer for
@bold{Mac OS (Intel 64-bit)} when
@link["https://download.racket-lang.org/"]{downloading Racket}.  Do
not use Apple Silicon 64-bit installer.  This will work thanks to
Rosetta.

Otherwise, follow the steps given above.

@section[#:tag "Windows"]{Using Windows}

For Windows users, using WSL for testing is highly recommended. Beyond
the first few assignments, the projects will require generating and
executing assembly code using the nasm package. Students in the past
have had trouble trying to configure this in the Windows environment,
so an easier workaround is simply to enable WSL and run your tests through
some Linux Distribution. Here is a breakdown of the steps:

@itemlist[
 #:style 'ordered
 @item{Following the instructions at
  @link["https://docs.microsoft.com/en-us/windows/wsl/install-win10"]{
   this link}, install a Linux Distro of your choice (e.g.,
  Ubuntu). The instructions include a suggestion to upgrade to
  WSL2; this is not necessary but will improve efficiency in
  general.}

 @item{Open your installed Linux distribution of choice and
  make any initial configurations necessary (user, pass,
  etc.). Run @tt{sudo apt update} and follow with @tt{sudo apt
   upgrade}. These two may take some time. }

 @item{Run @tt{sudo apt install racket} and @tt{
   sudo apt install nasm}. These two should cover the necessary
  installations for this course.}

 @item{Here is where to determine which IDE you would like to
  use.

@itemlist[
  @item{Using vim (or Emacs as mentioned in the previous section) is simple.
   Copy assignment files into WSL. Modify files. }

  @item{Previous students preferred installing VSCode (outside of WSL) from
   @link["https://code.visualstudio.com/download"]{this link}. For each
   assignment, copy assignment files somewhere on your Linux distro. If you
   would like to open @tt{some-file.rkt}, you can open it from the command line by
   calling @tt{code some-file.rkt} and, after some automatic set up, VSCode should
   load up the file. You can install Racket extensions from the VSCode
   Marketplace (a suggestion will also pop up once you open a .rkt file) to
   have colorized syntax, bracket matching, autocomplete/IntelliSense, etc. }

  @item{If you are intent on using DrRacket, you will want to set up an X
   Window System to run GUI programs from within WSL. A popular option is
   @link["https://mobaxterm.mobatek.net/"]{MobaXterm}. Once that is installed,
   you can launch DrRacket from within your WSL terminal by running the
   command @tt{drracket}.

   You could also install DrRacket @emph{outside} WSL and copy your files back
   and forth or use a symbolic link to connect the two. However, DrRacket will
   not be able to see the @tt{langs} package you will install later by default,
   so you would either need to install it again within DrRacket or else find a
   way to connect your Windows-based DrRacket to your WSL-based Racket package
   configuration. This seems unnecessarily complicated, though, so we don't
   recommend this option. }
]}

]

Regardless of the IDE used, you can now run your tests from your Linux
subsystem by entering the project directory and using the raco command.

@section[#:tag "install-racket"]{Installing Racket}

Racket is available for all major operating systems from:

@centered{@link["https://racket-lang.org/"]{@tt{https://racket-lang.org/}}}

We will be using Racket @racket-version, but any version from the past several
years should work fine.

There are two essential references:

@itemlist[
@item{@link["https://docs.racket-lang.org/guide/"]{The Racket Guide} - intended for those new to Racket, i.e. @emph{you!}}
@item{@link["https://docs.racket-lang.org/reference/"]{The Racket Reference} - the definitive, comprehensive manual for Racket}
]

Racket is a large, full-featured, batteries-included language
platform.  However, we will be using only a small subset of Racket.
This subset should be easy to pick up for anyone familiar with
functional programming.  If you're comfortable with basic OCaml,
Haskell, or even JavaScript, you shouldn't have much trouble learning
the Racket bits we will be using.

@section[#:tag "langs-package"]{The @tt{langs} package}

After installing Racket, install the @tt{langs} package
which includes course utilities such as the @secref{a86}
library.

To install, run the following command:

@verbatim|{raco pkg install --auto 'https://github.com/cmsc430/www.git?path=langs#main'}|

To test the package works as expected, run:

@verbatim|{raco test -p langs}|

All of the tests should pass; if they don't, consult course staff.

The package source is hosted on Github. To check for and
install updates, run:

@verbatim|{raco pkg update langs}|

@section{IDE}

Racket comes with its own IDE: DrRacket, which is the recommended way
to edit Racket files.  We will also be running Racket and its
associated tools from the command line.

If you'd like to use Emacs, there's a good
@link["https://www.racket-mode.com/"]{Racket mode}, but we recommend
using DrRacket for a while before switching to Emacs.  Using any other
editor is fine, too.

@;{
@section{Detailed compatiblity list}

The course software has been successfully tested with the
following:

@itemlist[
 @item{Operating systems:
  @itemlist[@item{Ubuntu 20.04}
            @item{Ubuntu 18.04}
            @item{Red Hat Enterprise Linux 7.7}
            @item{macOS 11.0 (Big Sur)}
            @item{macOS 10.15 (Catalina)}]}

 @item{Racket:
  @itemlist[@item{Racket 8.1 [cs]}
            @item{Racket 8.1 [bc]}
	    @item{Racket 8.0 [cs]}
            @item{Racket 8.0 [bc]}
	    @item{Racket 7.9 [cs]}
            @item{Racket 7.9 [bc]}
            @item{Racket 7.8 [cs]}
            @item{Racket 7.8 [bc]}]}

 @item{NASM:
  @itemlist[@item{NASM version 2.13.02}
            @item{NASM version 2.15.05}]}

 @item{GCC:
  @itemlist[@item{gcc 9.3.0}
            @item{gcc 7.5.0}
            @item{Clang/LLVM 12.0.0}]}]

@; DVH: I'm not sure this is useful.  The OCaml to Racket notes are better.
@;{

@section{Grammar}

A program is a sequence of definitions or expressions.

@(define unquote "whatever") @;{Needed to make redex happy with unquote in grammar}
@(define-language R0
  (d ::= (define x e) (define (x x ...) e))
  (e ::= (e e ...) (δ e ...) sv x (λ (x ...) e) (quasiquote qq) (match e [p e] ...))
  (qq ::= (qq ...) sv x (unquote e))
  (sv ::= b n s)
  (p ::= (quasiquote r) b n x s (cons p p))
  (r ::= b n x s (unquote p))
  (s ::= string)
  (b ::= #t #f)
  (n ::= integer)
  (x ::= variable)
  (δ ::= add1 sub1 = * + - list cons))

The grammar for the subset of Racket we will use is:

@(with-unquote-rewriter
  (lambda (lw)
    (build-lw (list (build-lw "(" (lw-line lw) (lw-line-span lw) (lw-column lw) 1)
                    (build-lw 'unquote (lw-line lw) (lw-line-span lw) (+ 1 (lw-column lw)) 7)
                    (build-lw " " (lw-line lw) (lw-line-span lw) (+ 2 (lw-column lw)) 1)
                    (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (+ 3 (lw-column lw)) (lw-column-span lw))
                    (build-lw ")" (lw-line lw) (lw-line-span lw) (lw-column lw) 1))
               (lw-line lw)
               (lw-line-span lw)
               (lw-column lw)
               (+ 8 (lw-column-span lw))))


  (render-grammar R0))

@section{Built-In Datatypes}

We will use:

@itemize[
@item{Booleans}
@item{Numbers}
@item{Strings}
@item{Symbols}
@item{Pairs and Lists}
]

We will make extensive use of @link["https://docs.racket-lang.org/guide/quote.html"]{@tt{quote}}.

@section{Definitions}

A definition takes the form:

@render-grammar/nts[R0 '(d)]

A definition @render-term[R0 (define x e)] defines @render-term[R0 x]
to stand for the value produced by evaluating @render-term[R0 e].

The @render-term[R0 (define (x_0 x_1 ...) e)] form is shorthand for
@render-term[R0 (define x_0 (λ (x_1 ...) e))].

@;{
@section{Style}

TODO: write style guidelines.
}

@section{Examples}

Here are some examples of writing various functions in our subset of Racket.

@#reader scribble/comment-reader
(ex

;; compute the product of a list of numbers
(define (prod xs)
  (match xs
    ['() 1]
    [(cons x xs) (* x (prod xs))]))

(prod '(1 2 3 4 5))

;; reverse a list
(define (rev xs)
  (rev/acc xs '()))

(define (rev/acc xs a)
  (match xs
    ['() a]
    [(cons x xs) (rev/acc xs (cons x a))]))

(rev '(1 2 3 4 5))
)

}

}