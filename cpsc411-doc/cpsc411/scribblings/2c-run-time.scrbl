#lang scribble/manual

@(require
  scribble/example
  (for-label
   cpsc411/2c-run-time
   racket/pretty
   (except-in racket/base compile)))

@(define eg (make-base-eval '(require cpsc411/2c-run-time)))

@title{2c Run-Time System}

@author[@author+email["William J. Bowman" "wjb@williamjbowman.com"]]
@defmodule[cpsc411/2c-run-time]

This library provides the run-time system for supporting the CPSC 411 languages
that can only produce two's complement integers.

@defproc[(wrap-x64-run-time (v string?)) string?]{
Wraps @racket[v], a string representing a sequence of x64 instructions in Intel
syntax, with the CPSC 411 run-time system.

This run-time system prints a two's complement integer to the standard output
port, as an ASCII string.

Implementation detail: currently, does nothing, as @racket[wrap-x64-boilerplate]
actually installs the run-time system, so this function cannot be used on its
own; @racket[wrap-x64-boilerplate] must be called immediately after.

Currently only supports Linux and macOS.

@examples[#:eval eg
(wrap-x64-run-time "mov rax, 5")
]
}

@defproc[(wrap-x64-boilerplate (e string?)) string?]{
Wraps @racket[e], a string representing a sequence of x64 instructions in Intel
syntax, with necessary boilerplate to compile via @tt{nasm}.

Implementation detail: currently, installs the run-time system as well.

Currently only supports Linux and macOS.

@examples[#:eval eg
(require racket/pretty)
(pretty-display (wrap-x64-boilerplate "mov rax, 5"))
]
}
