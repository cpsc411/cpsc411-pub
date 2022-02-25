#lang scribble/manual

@(require
  (for-label
   rackunit
   (except-in cpsc411/compiler-lib compile)
   (except-in racket/base read read-syntax)
   cpsc411/test-suite/public/v4
   cpsc411/test-suite/public/v5
   cpsc411/langs/v5
   racket/contract)
  cpsc411/langs/v5)

@title{Version 5 Test Suites}
@defmodule[cpsc411/test-suite/public/v5]

@defproc[(v5-public-test-suite [pass-list (listof (-> any/c any/c))]
                               [interp-list (listof (or/c #f (-> any/c any/c)))]
                               [check-values-lang (-> any/c values-lang-v5?)])
          test-suite?]{
The test suite for the v5 compiler passes.
Reuses all test suites from @racket[v4-public-test-suite] where possible.

@racket[pass-list] is expected to be a list of passes that compile from
@racket[values-lang-v5?] to x64, compatible with @racket[current-pass-list].

See @racket[v1-public-test-suite] for details about the interpretation of
@racket[pass-list] and @racket[interp-list].

Expects 1 function not directly part of compilation separate, and performs
various kinds of property-based testing on it.
}


