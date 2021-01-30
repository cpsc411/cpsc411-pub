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
   racket/contract)
  scribble/examples)

@title{Testing}

@section{Test Suites}
This section describes the public tests suites provided by the library.
See @racket[test-suite], @racketmodname[rackunit], @racket[run-tests], and @racketmodname[rackunit/text-ui].

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
