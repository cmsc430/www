#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@title[#:tag "Graphviz"]{Using Graphviz/dot to visualize our AST}

@table-of-contents[]

@section[#:tag-prefix "graphviz"]{Visualizing ASTs}

Abstract Syntax Trees (ASTs) are a useful abstraction when dealing with
programming languages as an object for analysis or manipulation (e.g.
compilation). At the same time, these structures can quickly become
too large reason about just by looking at it. For example, in Knock,
our AST for @tt{(if (zero? x) (add1 (add1 x)) (sub1 x))} looks
like the following:

@#reader scribble/comment-reader
(racketblock
 (if-e
  (prim-e 'zero? (list (var-e 'x)))
  (prim-e 'add1 (list (prim-e 'add1 (list (int-e 1)))))
  (prim-e 'sub1 (list (var-e 'x)))))

This has all the information necessary for manipulating our program (and more),
it's a bit unwieldy to look at. Particularly when debugging, it can
be useful to see the overal @emph{shape} of the AST. This is particularly
true when speaking about program transformations.

Take, for example, the program transformation from the first Midterm. Applying
that transformation (updated for Knock) to the above program, results in
the following AST:

@#reader scribble/comment-reader
(racketblock
 (if-e
  (prim-e 'zero? (list (var-e 'x)))
  (let-e
   (list (binding 'g387 (prim-e 'add1 (list (int-e 1)))))
   (prim-e 'add1 (list (var-e 'g387)))
  (prim-e 'sub1 (list (var-e 'x))))))

Was the program transformation done correctly? If you study the AST
carefully, you can determine that it was. However, it would be easier
if we could, at a glance, answer the question ``Are primitive operations
only applied to simple (i.e. not nested) expressions?''

Using diagrams makes answering this question marginally easier:

Before transformation:

@image{img/initial.png}

After transformation:

@image{img/transformed.png}

The diagram above helps us visualize the transformed AST, but we still
have to study the diagram carefully to know which nodes correspond
to primitive operations (which are the subject of the transformation).
This can be remedied easily, by coloring these nodes differently:

Before transformation:

@image{img/initial-v2.png}

After transformation:

@image{img/transformed-v2.png}

These diagrams were made using @tt{dot} a tool provided by the
@link["https://graphviz.org/"]{Graphviz}, which is a set of software components
for visualizing graphs.

@section[#:tag-prefix "graphviz"]{Using dot}

Graphviz has many components, but we will focus on @tt{dot}, which is
the tool for laying out directed graphs. The full manual for @tt{dot}
can be found on the graphviz website:
@link["https://www.graphviz.org/pdf/dotguide.pdf"]{The Dot Guide}.

Instructions for downloading Graphviz (and therefore @tt{dot}) can be found on
their website as well:
@link["https://www.graphviz.org/download/"]{Download Graphviz}

The syntax for @tt{dot} files is fairly straightforward, you first declare the
type of graph, and give it a name. For our purposes the type will always be
@tt{digraph} (i.e. directed-graph), and the name can be whatever you choose
(though it will likely not matter much). For example:

@verbatim|{
digraph CMSC430 {
  ...
}
}|

The ellipses are where you describe the graph you'd like to visualize. The
designers of Graphviz provide a grammar describing the language accepted by
their tools (I wish all system designers provided a grammar!). This can be
found on the Graphviz website:
@link["https://graphviz.org/doc/info/lang.html"]{The DOT language}.

Most of the time you will not need to consult the grammar, as most of the
simple rules are straightforward for those that have programmed in C or Java.

In short, the description of a graph is a list of statements, statements can
take many forms, but for this course (and most likely for any uses beyond this
course), you can basically just use the following three types of statements:


@itemlist[

@item{Node statements}
@item{Edge statements}
@item{Attribute statements}

]


Node statements are just an ASCII string (representing a Node ID) and an
optional list of attributes for that node. For example:

@verbatim|{
digraph CMSC430 {
  lexer;
  parser [shape=box];
  code_gen [color=red];
}
}|

Using the @tt{dot} tool on a file with the above as its contents produces the
following diagram:

@image{img/nodes.png}

Edge statements connect nodes in our graph, for example:

@verbatim|{
digraph CMSC430 {
  lexer -> parser -> code_gen;
  parser [shape=box];
  code_gen [color=red];
}
}|

This produces the following diagram:

@image{img/edges1.png}

You may wonder if the order matters here. While the @emph{horizontal} order
matters when specifying the edges in an edge statement, the @emph{vertical}
order does not matter in this case. The following produces the same diagram:

@verbatim|{
digraph CMSC430 {
  parser [shape=box];
  code_gen [color=red];
  lexer -> parser -> code_gen;
}
}|

Notice that @tt{lexer} does not have its own `declaration' this is because it
is unnecessary unless you want to attach attributes to a node (as we do
with @tt{parser} and @tt{code_gen}).

Edge statements also support an optional list of attributes, the following
produces a similar diagram except that both edges are shaded ``deeppink2'' (for
the full list of supported colors, see the official documentation).

@verbatim|{
digraph CMSC430 {
  lexer -> parser -> code_gen [color=deeppink2];
  parser [shape=box];
  code_gen [color=red];
}
}|

Attribute nodes describe a set of attributes that apply to all subsequent
statements (which means that vertical order @emph{does} matter here!). Unless
overridden by a specific attribute, all statements following an attribute
statement will `default' to the attributes specified in the statement.

Here we added three attribute statements. Take a minute to study the example
below and see how each attribute statement affects the output.


@verbatim|{
digraph CMSC430 {
  edge [color=blue];
  lexer -> parser
  edge [color=deeppink2];
  node [shape=triangle];
  parser -> optimizer;
  parser [shape=box];
  code_gen [color=red];
  optimizer -> code_gen;
}
}|

@image{img/edges3.png}


@section[#:tag-prefix "graphviz"]{Using graphviz programmatically}

What we've done is write a small Racket library that abstracts away some of the
details of making @tt{dot} diagrams so that we can automatically generate
digrams from our AST. One such detail is that we have to generate unique node
IDs for each node in our AST (we do this using @tt{gensym}), but then add
attributes that label our nodes with the relevant information (e.g. that it's
an @tt{if} node).

Here is an example of a @tt{dot} description make using our library on the program
@racket[(if (zero? x) 1 2)]:

@verbatim|{
digraph prog {
  g850 [ label=" x " ];
  g849 [ color=red,label=" (zero? ...) " ];
  g849 -> g850 ;
  g851 [ label=" 1 " ];
  g852 [ label=" 2 " ];
  g848 [ label=" if " ];
  g848 -> g849 ;
  g848 -> g851 ;
  g848 -> g852 ;
}
}|

Not super nice to read, but we had a program write it for us!


The complete library (three files):

@codeblock-include["knock/dot.rkt"]
@codeblock-include["knock/render-ast.rkt"]
@codeblock-include["knock/pretty-printer.rkt"]


