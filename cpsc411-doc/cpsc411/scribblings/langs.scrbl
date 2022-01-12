#lang scribble/manual

@(require scribble/example)

@title[#:tag "ref-langs"]{Intermediate Language Reference Implementations}

@author[@author+email["William J. Bowman" "wjb@williamjbowman.com"]]

This library provides reference implementations of each intermediate language,
including interpreters and validators.
Valid programs should run correctly in the interpreters, although so will some
invalid programs.
Validators will reject invalid programs, but not necessarily provide useful errors.

@section{v1 Languages}
@defmodule[cpsc411/langs/m1]
@(define egv1 (make-base-eval '(require cpsc411/compiler-lib cpsc411/langs/m1)))

@defproc[(interp-paren-x64-v1 [v paren-x64-v1?]) (in-range/c 0 256)]{
The reference interpreter for Paren-x64 v1; produces the final value of @tt{rax}
as an exit code (modulo 256).

@examples[#:eval egv1
(interp-paren-x64-v1 `(begin (set! rax 42)))
]
}
