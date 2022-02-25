#lang scribble/manual

@(require
  (for-label
   rackunit
   (except-in cpsc411/compiler-lib compile)
   (except-in racket/base read read-syntax)
   cpsc411/test-suite/public/v5
   cpsc411/test-suite/public/v6
   cpsc411/langs/v6
   racket/contract)
  cpsc411/langs/v6
  scribble/example)


@title{Version 6 Test Suites}
@defmodule[cpsc411/test-suite/public/v6]

@(define sb (make-base-eval '(require cpsc411/test-suite/public/v6)))

@defproc[(v6-public-test-suite [pass-list (listof (-> any/c any/c))]
                               [interp-list (listof (or/c #f (-> any/c any/c)))])
          test-suite?]{
The test suite for the v6 compiler passes.
Reuses all test suites from @racket[v5-public-test-suite] where possible.

@racket[pass-list] is expected to be a list of passes that compile from
@racket[values-lang-v6?] to x64, compatible with @racket[current-pass-list].

See @racket[v1-public-test-suite] for details about the interpretation of
@racket[pass-list] and @racket[interp-list].
}

@defproc[(replace-halts [p 'a] [done symbol? 'r15]) 'a]{
Given a program @racket[p] in some language @racket['a], replaces all instances
of @asm-pred-lang-v6[(halt opand)] with
@asm-pred-lang-v6[(begin (set! rax opand) (jump _done))], where @racket[done] is
the second argument to @racket[replace-halts].

This can be used to update v5 tests that are now incompatible with the new
v6 languages.

@examples[#:eval sb
(replace-halts '(module (halt 5)))
]
}

@defproc[(replace-implicit-return [p 'a]) 'a]{
Given a program @racket[p] in some language @racket['a], replace the final
implicit return value (the final value in tail position) with an explicit
return, i.e., @asm-pred-lang-v6[(begin (set! rax value) (jump r15))].

This can be used to update v5 tests that are now incompatible with the new v6
languages.

@examples[#:eval sb
(replace-implicit-return '(module (begin (set! x.1 6) x.1)))
]
}

@defproc[(add-new-frames [p 'a]) 'a]{
Given a program @racket[p] in some language @racket['a], add an empty
@asm-pred-lang-v6[new-frames] field to the info fields.

This can be used to update v5 tests that are now incompatible with the new v6
languages.
All v5 programs have empty new frames, since they only supported tail calls.

@examples[#:eval sb
(add-new-frames '(module () (define L.id.1 () (begin (set! x.1 r9) (halt x.1)))
  (call L.id.1 5)))
]
}

@defproc[(fixup-begin [p 'a]) 'a]{
Given a program @racket[p] in @racket[para-asm-lang-v6], fixup any nested begin
introduced by @racket[replace-halts].

This can be used to update v5 tests that are now incompatible with the new v6
languages.

@examples[#:eval sb
(replace-halts '(begin (halt 5)))
(fixup-begin (replace-halts '(begin (halt 5))))
]
}
