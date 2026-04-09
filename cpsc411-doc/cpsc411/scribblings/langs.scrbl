#lang scribble/manual

@(require
  scribble/example
  (for-label racket/base))

@title[#:style 'toc #:tag "ref-langs"]{Language Reference Implementations}

This library provides reference implementations of each language, including
interpreters and validators.
Valid programs should run correctly in the interpreters, although so will some
invalid programs.
Validators will reject invalid programs, but don't necessarily provide useful errors.

The interpreters have a few configuration parameters:

@defmodule[cpsc411/langs/base]
@(require (only-in cpsc411/langs/base current-heap-size current-stack-size))
@defparam[current-heap-size heap-size exact-nonnegative-integer? #:value #,(current-heap-size)]{
Set the size of the heap for the interpreter.

@examples[
(require (only-in cpsc411/langs/base current-heap-size))
(require cpsc411/langs/v8)
(current-heap-size 20000)
(interp-asm-pred-lang-v8
 '(module
    ((new-frames (() ())))
    (begin
     (set! x.1 r12)
     (set! r12 (+ r12 10008))
     (mset! x.1 10000 5)
     (mref x.1 10000))))
]
}

@defparam[current-stack-size heap-size exact-nonnegative-integer? #:value #,(current-stack-size)]{
Set the size of the heap for the interpreter.
}


@include-section{langs/v1.scrbl}
@include-section{langs/v2.scrbl}
@include-section{langs/v2-reg-alloc.scrbl}
@include-section{langs/v3.scrbl}
@include-section{langs/v4.scrbl}
@include-section{langs/v5.scrbl}
@include-section{langs/v6.scrbl}
@include-section{langs/v6-5.scrbl}
@include-section{langs/v7.scrbl}
@include-section{langs/v8.scrbl}
@include-section{langs/v9.scrbl}
@include-section{langs/v10.scrbl}
@include-section{langs/v11.scrbl}
