#lang scribble/manual

@(require
  (for-label
   rackunit
   (except-in cpsc411/compiler-lib compile)
   (except-in racket/base read read-syntax)
   cpsc411/test-suite/public/v1
   cpsc411/langs/v1
   racket/contract)
  cpsc411/langs/v1
  scribble/examples)

@title{Version 1 Test Suites}
@defmodule[cpsc411/test-suite/public/v1]

@defproc[(v1-public-test-suite [pass-list (listof (-> any/c any/c))]
                               [interp-list (listof (or/c #f (-> any/c any/c)))]
                               [check-paren-x64 (-> any/c paren-x64-v1?)]
                               [interp-paren-x64 (-> paren-x64-v1? (in-range 0 255))])
         test-suite?]{
The test suite for the v1 compiler passes.

@racket[pass-list] is expected to be a list of passes that compile from
@racket[paren-x64-v1?] to x64, compatible with @racket[current-pass-list].

@racket[interp-list] must be the same length as @racket[pass-list], and should
be a list of interpreters for the source language of the corresponding pass in
@racket[pass-list], or @racket[#f] to indicate this pass has no interpreter and
cannot be independently tested.
Such passes will be composed together and tested against the next available
interpreter.
The test suite uses @racket[execute] with an empty pass list as the final target
language interpreter.

For example, given a pass list @racket[(list check-paren-x64 generate-x64
wrap-x64-run-time wrap-x64-boilerplate)] and interpreter list
@racket[(list interp-paren-x64 interp-paren-x64 #f #f)],
@racket[check-paren-x64] will be tested by interpretering both the input and
output with @racket[interp-paren-x64].
The passes @racket[generate-x64], @racket[wrap-x64-run-time], and
@racket[wrap-x64-boilerplate] are composed together, and tested by interpreting
source programs with @racket[interp-paren-x64], and target programs through
execution.
}

