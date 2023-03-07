#lang scribble/manual

@(require
  (for-label
   rackunit
   (except-in cpsc411/compiler-lib compile)
   (except-in racket/base read read-syntax)
   cpsc411/test-suite/public/v4
   cpsc411/langs/v4
   racket/contract)
  cpsc411/langs/v4
  scribble/examples)

@title{Version 4 Test Suites}
@defmodule[cpsc411/test-suite/public/v4]

@defproc[(v4-public-test-suite [pass-list (listof (-> any/c any/c))]
                               [interp-list (listof (or/c #f (-> any/c any/c)))]
                               [link-paren-x64 (or/c #f (-> paren-x64-v4 paren-x64-rt-v4?))]
                               [interp-paren-x64 (or/c #f (-> paren-x64-v4? int64?))]
                               [interp-values-lang (or/c #f (-> values-lang-v4? int64?))]
                               [check-values-lang (or/c #f (-> any/c values-lang-v4?))])
         test-suite?]{
The test suite for the v4 compiler passes.
Reuses all test suites from @racket[v3-public-test-suite] where possible.

@racket[pass-list] is expected to be a list of passes that compile from
@racket[values-lang-v4?] to x64, compatible with @racket[current-pass-list].

See @racket[v1-public-test-suite] for details about the interpretation of
@racket[pass-list] and @racket[interp-list].

Expects 4 optional functions not directly part of compilation separate, and
performs various kinds of property-based testing on them, if they are given.
}
