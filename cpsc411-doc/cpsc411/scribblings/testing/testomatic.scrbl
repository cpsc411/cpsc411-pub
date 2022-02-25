#lang scribble/manual
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

@title{Compiler Testomatic}
@defmodule[cpsc411/test-suite/utils]
This section describes the lower-level interface to the property-based test
suites described in the next section.
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
                             [trg-interp ('b -> 'c)]
                             [trg-validator ((or/c any/c 'b) -> boolean?)])
                             void?]{
@racket['a], @racket['b], and @racket['c] represent arbitrary non-necessarily
distinct type variables.
This assumes @racket['c] is some type with Racket values compared by @racket[equal?].

Takes a compiler pass from some language @racket['a] to some language
@racket['b], an interpeter for each language, and a validator that recognizes
program in the language @racket['b].
Run @racket[pass] on each test source program registered with the framework, and
compares the results in the respective interpreters.
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
   (code:comment "Compiles programs to themselves; 2 tests fail. since it contains fvars")
   (lambda (x) x)
   (code:comment "Compiles programs to 120; 0 tests fails.")
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
Expects an interpreter provided directly from @racketmodname[cpsc411/langs], and a
list of test programs.
The test programs are a proper-list whose @racket[first] is a string
representing a name and whose @racket[second] is a valid test program in
@racket[src-interp].

The test programs are registered for later use in the testomatic framework.
}

