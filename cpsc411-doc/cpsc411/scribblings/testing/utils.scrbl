#lang scribble/manual

@(require (for-label racket/base racket/contract racket/engine cpsc411/test-suite/utils rackunit))

@title{Test Suite Utils}
@defmodule[cpsc411/test-suite/utils]

@defparam[current-test-case-timeout timeout natural-number/c #:value 5000]{
A number of milliseconds used as a timeout by the testing infrastructure when running a test that might diverge.
Used directly by @racket[with-timeout] and @racket[run-test-with-timeout], and
indirectly by @racket[test-compiler-pass], and @racket[compiler-testomatic].
}

@defproc[(run-test-with-timeout
          [proc (-> any/c)]
          [timeout-ms natural-number/c (current-test-case-timeout)]
          [fail-k (-> any/c) (lambda () (fail-check "Time out"))])
          any/c]{
Run the thunk @racket[proc] (expected to run a @racket[test-case] or a
@racket[check]), in an @racket[engine], either capturing and
returning the result, or running @racket[fail-k] if the engine times out.
}

@defform[(with-timeout stx ...)]{
Elaborates to @racket[(run-test-with-timeout (lambda () stx ...))].
}

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
