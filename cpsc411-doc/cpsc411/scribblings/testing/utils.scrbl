#lang scribble/manual

@(require (for-label racket/base racket/contract))

@title{Test Suite Utils}
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
