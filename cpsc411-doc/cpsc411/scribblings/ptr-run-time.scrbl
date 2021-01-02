#lang scribble/manual

@(require
  scribble/example
  (for-label
   cpsc411/ptr-run-time
   (except-in racket/base compile)
   racket/pretty))

@(define eg (make-base-eval '(require cpsc411/ptr-run-time)))

@title{Ptr Run-Time System}

@author[@author+email["William J. Bowman" "wjb@williamjbowman.com"]]
@defmodule[cpsc411/ptr-run-time]

This library provides the run-time system for supporting the CPSC 411 languages
that can print ptr-encoded data.

@defproc[(wrap-x64-run-time (v string?)) string?]{
Wraps @racket[v], a string representing a sequence of x64 instructions in Intel
syntax, with the CPSC 411 run-time system.

This run-time system prints the final return value, which must be a ptr encoded
data type, to the standard output port, as an ASCII string.

Currently only supports Linux and macOS.
@examples[#:eval eg
(require racket/pretty)
(pretty-display (wrap-x64-run-time "mov rax, 5"))
]
}

@defproc[(wrap-x64-boilerplate (e string?)) string?]{
Wraps @racket[e], a string representing a sequence of x64 instructions in Intel
syntax, with necessary boilerplate to compile via @tt{nasm}.

Currently only supports Linux and macOS.

@examples[#:eval eg
(require racket/pretty)
(pretty-display (wrap-x64-boilerplate "mov rax, 5"))
]
}
