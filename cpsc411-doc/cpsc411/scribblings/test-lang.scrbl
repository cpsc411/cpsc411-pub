#lang scribble/manual

@(require
  (for-label
   rackunit
   rackunit/text-ui
   cpsc411/test-suite/utils
   cpsc411/graph-lib
   (except-in cpsc411/compiler-lib compile)
   (except-in racket/base read read-syntax)
   cpsc411/test-suite/public/a1
   cpsc411/langs/v1
   cpsc411/langs/v2
   cpsc411/langs/v3
   racket/contract)
  scribble/examples)

@title{Testing}

@section{Test Suites}
This section describes the public tests suites provided by the library.
See @racket[test-suite], @racketmodname[rackunit], @racket[run-tests], and @racketmodname[rackunit/text-ui].

There are currently two versions of test suites:
@itemlist[
@item{Largely syntactic unit tests for each milestone.
These are based on the milestones, not the chapter, and are fragile.
They make assumptions about the structures of pass list, and can break if
students use a different structures.
They are deprecrated and exist for backwards compatibility.}
@item{Property-based test suites for each chapter.
These are based on the language versions and are much more robust, as they test
via the language interpreters and validators.
These make no assumptions about the structure of the pass list.
They are strongly recommended.

The suites work best when the compiler is designed and implementated top-down,
and may not test intermediate passes until earlier passes are complete, as the
testing framework generates tests for later passes from earlier passes that pass
their own tests.
}
]

@(local-table-of-contents #:style 'immediate-only)

@subsection{Version 1 Test Suites}
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

@subsection{Version 2 Test Suites}
@defmodule[cpsc411/test-suite/public/v2]

@defproc[(v2-public-test-suite [pass-list (listof (-> any/c any/c))]
                               [interp-list (listof (or/c #f (-> any/c any/c)))])
         test-suite?]{

The test suite for the v2 compiler passes.

Reuses all test suites from @racket[v1-public-test-suite] where possible.

@racket[pass-list] is expected to be a list of passes that compile from
@racket[asm-lang-v2?] to x64, compatible with @racket[current-pass-list].

See @racket[v1-public-test-suite] for details about the interpretation of
@racket[pass-list] and @racket[interp-list].
}



@subsection{Version 3 Test Suites}
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


@subsection{Milestone 1 Test Suites}
@defmodule[cpsc411/test-suite/public/a1]

@defproc[(a1-public-test-suite [pass-list (listof (-> any/c any/c))]
                               [interp (-> paren-x64? integer?)])
         test-suite?]{
The Milestone 1 test suite.
@racket[pass-list] is expected to be a list containing the milestone 1
functions, in order: @racket[check-paren-x64], @racket[generate-x64],
@racket[wrap-x64-run-time], and @racket[wrap-x64-boilerplate].
@racket[interp] is expected to be the function @racket[interp-paren-x64].
}

@subsection{Milestone 2 Test Suites}
@defmodule[cpsc411/test-suite/public/a2]

@defproc[(a2-public-test-suite [pass-list (listof (-> any/c any/c))]
                               [uncover-locals (-> asm-lang-v2? asm-lang-v2/locals?)]
                               [assign-fvars (-> asm-lang-v2/locals? asm-lang-v2/assignments?)]
                               [replace-locations (-> asm-lang-v2/assignments? nested-asm-lang-v2?)]
                               [check-paren-x64 (-> any/c paren-x64-v2?)]
                               [interp-values-lang (-> values-lang-v3? int64?)]
                               [interp-paren-x64 (-> paren-x64-v2? int64?)])
         test-suite?]{
The Milestone 2 test suite.
@racket[pass-list] is expected to be a list containing the milestone 2
functions, in order.
The remaining arguments are expected to be the correspondingly named passes,
which are not directly included in the pass list.
}



@section{Test Lang}
@defmodulelang[cpsc411/test-lang]
This module defines a language with a small reader extension to simplify writing
future-proof tests.

The language adds the reader macro @as-index{@litchar{$}}, which behaves like
@litchar{,} (@racket[unquote]), but calls the @racket[current-input-encoder].
to encode a Racket value into a valid value for the CPSC411 compiler.
For much of the compiler, this is simply the identity function: Racket numbers
in the int64 range are also CPSC411 values.
Once we add tagged datatypes, this encoding changes.

You can use @racketmodname[cpsc411/test-lang] only as a @litchar{#lang}.

For example, the following module
@codeblock{
#lang cpsc411/test-lang
`(begin (set! rax 1))
`(begin (set! rax $1))
'(begin (set! rax $1))
(parameterize ([current-input-encoder (lambda (x) (* x 8))])
  `(begin (set! rax $1)))
}

Reads the same as:
@(define eg (make-base-eval))
@examples[#:eval eg
(require
 cpsc411/test-suite/utils)
`(begin (set! rax 1))
`(begin (set! rax ,((current-input-encoder) 1)))
'(begin (set! rax ,((current-input-encoder) 1)))
(parameterize ([current-input-encoder (lambda (x) (* x 8))])
  `(begin (set! rax ,((current-input-encoder) 1))))
]

@section{Test Suite Utils}
@defmodule[cpsc411/test-suite/utils]
@defparam[current-input-encoder encoder (-> any/c any/c) #:value values]{
An encode capable of transforming a Racket value into a valid value in the
current CPSC411 language.
}

@defparam[current-output-decoder decoder (-> any/c any/c) #:value values]{
A reader, capable of decoding a CPSC411 value either from an interpreter or from
compilation into a Racket value.
Typically, you should use @racket[execute] and @racket[current-run/read]
instead.
}

@defparam[current-expected-masker masker (-> any/c any/c) #:value values]{
A masker, masking an expected test Racket value to match the return value
interface of the CPSC411 compiler.
Probably either the default or set to @racket[exit-code-mask] for a1.
}

@defproc[(exit-code-mask [v any/c]) (between/c 0 255)]{
Mask a Racket value to the range of an exit code.
}

@defproc[(check-validator [f (-> any/c any/c)] [x any/c]) void?]{
Tests that the validator @racket[f] returns @racket[x] without raising an error.
}

@defproc[(check-validator-exn [f (-> any/c any/c)] [x any/c]) void?]{
Tests that the validator @racket[f] raises an exception when called on
@racket[x].
}
