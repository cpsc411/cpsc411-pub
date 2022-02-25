#lang scribble/manual

@(require
  (for-label
   rackunit
   rackunit/text-ui
   cpsc411/test-suite/utils))

@title[#:style 'toc]{Test Suites}
This section describes the public tests suites provided by the library.
See @racket[test-suite], @racketmodname[rackunit], @racket[run-tests], and @racketmodname[rackunit/text-ui].

These are property-based test suites for each chapter.
They test compiler passes via the language interpreters and validators.
These make no assumptions about the structure of the pass list.

The suites work best when the compiler is designed and implementated top-down,
and may not test intermediate passes until earlier passes are complete, as the
testing framework generates tests for later passes from earlier passes that pass
their own tests.

@(local-table-of-contents #:style 'immediate-only)

@include-section{v1.scrbl}
@include-section{v2.scrbl}
@include-section{v3.scrbl}
@include-section{v4.scrbl}
@include-section{v5.scrbl}
@include-section{v6.scrbl}
