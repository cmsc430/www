#lang scribble/manual
@(require scribble/core racket/list)

@title[#:style 'unnumbered]{Schedule}

@;(TuTh 2-3:15 CSI 2117)

@local-table-of-contents[]

@section[#:style 'unnumbered]{Lecture Material}

@(define (wk d) (nonbreaking (bold d)))

@tabular[#:style 'boxed
         #:sep @hspace[1] 
         #:row-properties '(bottom-border)
         (list (list @bold{Week} @bold{Tuesday} @bold{Thursday})
               (list @wk{Aug 27} 
                     @elem{Outlook; Typed Racket; Redex}
                     @elem{Operational Semantics; Interpreters})
               
               (list @wk{Sep 3} 
                     @elem{@link["slides/02-ocaml.pdf"]{OCaml}}
                     @elem{@link["slides/03-lexing-parsing.pdf"]{Lexing and Parsing}, @link["03-examples.tar.gz"]{03-examples.tar.gz}, @secref{proj1}})
               
               (list @wk{Sep 10} 
                     @elem{Lexing and Parsing} 
                     @elem{Lexing and Parsing}) ; @link["i1.pdf"]{In-class exercise}
               
               (list @wk{Sep 17} 
                     @elem{Lexing and Parsing}  ; @link["i1b.pdf"]{In-class exercise}
                     @elem{@link["slides/04-op-sem.pdf"]{Operational Semantics}, @link["04-op-sem.ml"]{04-op-sem.ml}})
               
               (list @wk{Sep 24}
                     @elem{Operational Semantics, @secref{proj2}} ; @link["i2.pdf"]{In-class exercise}
                     @elem{Operational Semantics})
               
               (list @wk{Oct 1}
                     @elem{@link["slides/05-ir-bytecode.pdf"]{Intermediate Representations}}
                     @elem{@bold{Midterm 1}})
               
               (list @wk{Oct 8} 
                     @elem{@link["slides/06-codegen.pdf"]{Code Generation}, @secref{proj3}}
                     @elem{Code Generation})
               
               (list @wk{Oct 15}
                     @elem{Code Generation, @link["06-codegen-1.ml"]{06-codegen-1.ml}, @link["06-codegen-2.ml"]{06-codegen-2.ml}}
                     @elem{@link["slides/07-optimization.pdf"]{Optimization}, @link["i4.pdf"]{In-class exercise}})
               
               (list @wk{Oct 22} @emph{Spring break: no class} @emph{Spring break: no class})
               
               (list @wk{Oct 29}
                     @elem{Optimization, @link["slides/08-data-flow.pdf"]{Data Flow Analysis}}
                     @elem{Data Flow Analysis,
          @link["se.ml"]{se.ml}, @link["se2.ml"]{se2.ml}})
               
               (list @wk{Nov 5} 
                     @elem{@link["slides/10-types.pdf"]{Type Systems}}
                     @elem{Type Systems, @link["10-types.ml"]{10-types.ml}})
               
               (list @wk{Nov 12} 
                     @elem{Type Systems}
                     @elem{@bold{Midterm 2}})
               
               (list @wk{Nov 19}
                     @elem{@link["slides/xx-symbolic-exec.pdf"]{Symbolic Execution}}
                     @elem{Type Systems})
               
               (list @wk{Nov 26} @elem{Type Systems} @elem{Thanksgiving})
               (list @wk{Dec 3} @elem{Defunctionalization, CPS, @link["12-cps-defun.ml"]{12-cps-defun.ml}} "Defunctionalization, CPS"))]
