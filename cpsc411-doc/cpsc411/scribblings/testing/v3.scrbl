#lang scribble/manual

@(require
  (for-label
   rackunit
   (except-in cpsc411/compiler-lib compile)
   (except-in racket/base read read-syntax)
   cpsc411/test-suite/public/v2
   cpsc411/langs/v3
   racket/contract)
  cpsc411/langs/v3
  scribble/examples)

@title{Version 3 Test Suites}
@defmodule[cpsc411/test-suite/public/v3]

@defproc[(v3-public-test-suite [pass-list (listof (-> any/c any/c))]
                               [interp-list (listof (or/c #f (-> any/c any/c)))])
          test-suite?]{
The test suite for the v3 compiler passes.
Reuses all test suites from @racket[v2-public-test-suite] where possible.

@racket[pass-list] is expected to be a list of passes that compile from
@racket[values-lang-v3?] to x64, compatible with @racket[current-pass-list].

See @racket[v1-public-test-suite] for details about the interpretation of
@racket[pass-list] and @racket[interp-list].
}

