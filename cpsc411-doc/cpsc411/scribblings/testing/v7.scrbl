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


@title{Version 7 Test Suites}
@defmodule[cpsc411/test-suite/public/v7]

@(define sb (make-base-eval '(require cpsc411/compiler-lib cpsc411/test-suite/public/v7)))

@defproc[(v7-public-test-suite [pass-list (listof (-> any/c any/c))]
                               [interp-list (listof (or/c #f (-> any/c any/c)))])
          test-suite?]{
The test suite for the v7 compiler passes.
Reuses all test suites from @racket[v6-public-test-suite] where possible.

@racket[pass-list] is expected to be a list of passes that compile from
@racket[values-lang-v6?] to x64, compatible with @racket[current-pass-list].

See @racket[v1-public-test-suite] for details about the interpretation of
@racket[pass-list] and @racket[interp-list].
}

@defproc[(i64->ptr [i int64?]) int64?]{
Convert an @racket[int64?] in twos-complement into the equivalent tagged, ptr representation.

@examples[#:eval sb
(i64->ptr 5)
(i64->ptr (max-int 64))
]
}

@defproc[(i32->ptr [i int32?]) int32?]{

Convert an @racket[int32?] in twos-complement into the equivalent tagged, ptr representation.

@examples[#:eval sb
(i32->ptr 5)
(i32->ptr (max-int 32))
]
}

@defproc[(replace-int64-exprs-to-ptr [p 'a]) 'a]{

Given a program @racket[p] in some language between @racket[exprs-bits-lang-v7] and
and @racket[proc-imp-cmf-lang-v7], convert any integer to a ptr representation.

@examples[#:eval sb
(replace-int64-exprs-to-ptr '(module 5))
(replace-int64-exprs-to-ptr '(module (if (= x 6) 5 6)))
]
}

@defproc[(replace-int-opands-to-ptr [p 'a]) 'a]{

Given a program @racket[p] in some imperative language with jumps and explicit
return, convert any integer operand to a ptr representation.

This version explicitly avoids opands that manipulate the
@racket[frame-base-pointer-register?], whose "integers" have a different
interpretation.

@examples[#:eval sb
(replace-int-opands-to-ptr '(module (begin (set! rax 5) (jump r15))))
(replace-int-opands-to-ptr '(module (begin (set! rbp (+ rbp 8)) (set! rax 5) (jump r15))))
]
}
