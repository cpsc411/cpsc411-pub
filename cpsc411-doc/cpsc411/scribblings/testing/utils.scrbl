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

@(require
  (for-label
   racket/list
   rackunit
   rackunit/text-ui
   cpsc411/test-suite/utils
   cpsc411/2c-run-time
   (except-in cpsc411/compiler-lib compile)
   (except-in racket/base read read-syntax)
   cpsc411/langs/v4
   cpsc411/langs/v4
   cpsc411/langs/v2
   racket/contract)
  scribble/example)

@section{Compiler Testomatic}
This section describes the lower-level interface to the property-based test
suites described in @secref["Test_Suites" #:doc '(lib "cpsc411/cpsc411.scrbl")].
This interface is a little unstable but may prove useful for property-based unit
testing.

The compiler testomatic framework keeps a map from language interpreters to test
programs.
Each pass is associated with an source interpreter and a target interpreter.
The pass is tested by running the source program in the source interpreter
against the output program in the target interpreter.
Any output program that is deemed valid is retained for testing later passes.

The framework also keeps an auxiliary map from interpreters to validators, and
will run validators before attempting to interpret output programs.

@(define eg
   (make-base-eval
    '(require
      racket/match
      rackunit
      rackunit/text-ui
      cpsc411/test-suite/utils
      cpsc411/langs/v4
      cpsc411/compiler-lib)))

@defproc[(test-compiler-pass [pass ('a -> 'b)]
                             [src-interp ('a -> 'c)]
                             [trg-interp ('b -> 'd)]
                             [trg-validator ((or/c any/c 'b) -> boolean?)]
                             [src-equiv ('c 'd -> boolean?) equal?])
                             void?]{
@racket['a], @racket['b], and @racket['c] represent arbitrary non-necessarily
distinct type variables.

Takes a compiler pass from some language @racket['a] to some language
@racket['b], an interpeter for each language, and a validator that recognizes
program in the language @racket['b].
Run @racket[pass] on each test source program registered with the framework, and
compares the results in the respective interpreters.
The results are compared using the optional @racket[src-equiv] procedure, which
defaults to @racket[equal?].
Note that all test suites documented in the next section implicitly register
tests with the framework.

@racket[test-compiler-pass] executes a sequence of @racket[test-case?], and so
should be run inside a @racket[test-suite], or it will have no effect.

@examples[#:eval eg
(define (uniquify p)
  (define (uniquify-tail tail)
    (match tail
      [`(let ([x ,v]) ,t)
       `(let ([x.1 ,v]) ,(uniquify-tail t))]
      ['x 'x.1]
      [_ tail]))
  (match p
    [`(module ,t)
     `(module ,(uniquify-tail t))]))

(test-compiler-pass uniquify interp-values-lang-v4 interp-values-lang-v4 values-unique-lang-v4?)

(run-tests
 (test-suite
  ""
  (test-compiler-pass uniquify interp-values-lang-v4 interp-values-unique-lang-v4 values-unique-lang-v4?)))

(register-test-programs!
 interp-values-lang-v4
 '(("" (module 5))
   ("" (module (let ([x 5]) x)))))

(run-tests
 (test-suite
  ""
  (test-compiler-pass uniquify interp-values-lang-v4 interp-values-unique-lang-v4 values-unique-lang-v4?)))

(run-tests
 (test-suite
  ""
  (test-compiler-pass values interp-values-lang-v4 interp-values-unique-lang-v4 values-unique-lang-v4?)))

(require cpsc411/test-suite/public/v4)
(run-tests
 (test-suite
  ""
  (test-compiler-pass uniquify interp-values-lang-v4 interp-values-unique-lang-v4 values-unique-lang-v4?))
 'quiet)

 (define (specify-implementation x)
   (match x [`(module ,x) `(module ,(* x 8))]))
 (require cpsc411/langs/v7)
 (register-test-programs!
  interp-exprs-unsafe-data-lang-v7
  '(("" (module 5))))
 (run-tests
  (test-suite
   ""
   (test-compiler-pass specify-implementation interp-exprs-unsafe-data-lang-v7
                       interp-exprs-bits-lang-v7 exprs-bits-lang-v7?
                       (lambda (sv tv) (equal? sv (ptr->v tv))))) 'quiet)
]
}

@defproc[(ptr->v [v ptr]) any/c]{
Converts a ptr (tagged 64-bit immediate data representation) to a Racket value.

@examples[#:eval eg
(ptr->v 8)
(ptr->v #b1110)
(ptr->v #b0110)
]
}

@(define eg2
  (make-base-eval
   '(require
     racket/match
     rackunit
     rackunit/text-ui
     cpsc411/test-suite/utils
     cpsc411/langs/v2
     cpsc411/compiler-lib
     cpsc411/2c-run-time)))

@defproc[(compiler-testomatic [passls (listof ('a -> 'b))]
                              [interpls (listof (or/c #f ('c -> any/c)))])
          test-suite?]{
Produces a @racket[test-suite] that tests each compiler pass in the
@racket[passls] using the interpreters given in the @racket[interpls], using all
test programs registered via @racket[register-test-programs!].

The lists are expected to have the same length, and the final pass in the
list must produce a string that can be assembled and executed using
@racket[execute] with an empty @racket[current-pass-list], i.e., it must be a
whole assembly program.

Each interpreter in the @racket[interpls] is expected to indicate the source
language interpreter for the respective pass in the @racket[passls].
That is, for a @racket[pass]in the @racket[passls] of type @racket[('a -> 'b)],
then corresponding interpreter must have the type @racket[('a -> any/c)].

Furthermore, the next interpreter in the list must exist and be the interpreter
for the target of @racket[pass], i.e, must have type @racket[('b -> any/c)], or
be @racket[#f] (unless the @racket[pass] is the final element of the
@racket[passls], in which case the target interpreter @racket[execute] is used).

If an interpreter is set to @racket[#f], that indicates the language is not
interpretable in isolation.
Instead, the pass is composed with the previous pass until a target language
interpreter can be found.

@examples[#:eval eg2
(register-test-programs!
 interp-paren-x64-v2
 '((""
    (begin
      (set! rax 120)))))

(register-test-programs!
 interp-paren-x64-fvars-v2
 '((""
    (begin
      (set! rax 120)))
   (""
    (begin
      (set! fv1 121)
      (set! rax fv1)))))

(run-tests
 (compiler-testomatic
  (code:comment "Compiles all programs to 120")
  (list (lambda (x) "mov rax, 120") wrap-x64-run-time wrap-x64-boilerplate)
  (list interp-paren-x64-v2 #f #f)))

(run-tests
 (compiler-testomatic
  (code:comment "Compiles all programs to 120")
  (list
   (code:comment "Compiles programs to themselves; 1 test fails, since it")
   (code:comment "contains fvars, thus the output isn't valid.")
   (lambda (x) x)
   (code:comment "Compiles programs to 120; 0 tests fails, since all")
   (code:comment "registered test programs in this language produce 120,")
   (code:comment "and no earlier passes produces new valid test programs.")
   (lambda (x) "mov rax, 120")
   wrap-x64-run-time
   wrap-x64-boilerplate)
  (list interp-paren-x64-fvars-v2 interp-paren-x64-v2 #f #f))
 )
]
}


@defproc[(register-test-programs! [src-interp (any/c -> any/c)]
                                  [test-progs (listof (list string? any/c))])
                                  void?]{
Expects an interpreter provided directly from @secref["ref-langs" #:doc '(lib "cpsc411/cpsc411.scrbl")] and a
list of test programs.
The test programs are a proper-list whose @racket[first] is a string
representing a name and whose @racket[second] is a valid test program in
@racket[src-interp].

The test programs are registered for later use in the testomatic framework.
}
