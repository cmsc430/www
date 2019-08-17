#lang scribble/manual
@title[#:style 'unnumbered #:tag "week2"]{Week 2: The First Few Compilers}

@table-of-contents[]

@include-section["abscond.scrbl"]
@include-section["blackmail.scrbl"]

@;{
An input source program is converted to an executable binary in many stages:

@itemlist[
@item{@bold{Parsed} into a data structure called an @bold{Abstract Syntax Tree}}
@item{@bold{Checked} to make sure code is well-formed (and well-typed)}
@item{@bold{Simplified} into some convenient @bold{Intermediate Representation}}
@item{@bold{Optimized} into (equivalent) but faster program}
@item{@bold{Generated} into assembly x86}
@item{@bold{Linked} against a run-time (usually written in C)}
]

}