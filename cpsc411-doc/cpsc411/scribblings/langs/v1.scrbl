#lang scribble/manual

@(require
  ;scribble/example
  "utils.rkt"
  cpsc411/langs/v1
  (for-label cpsc411/langs/v1)
  (for-label racket/contract))

@title{v1 Languages}
@defmodule[cpsc411/langs/v1]
@;(define egv1 (make-base-eval '(require cpsc411/compiler-lib cpsc411/langs/v1)))

@deflangs[
paren-x64-v1
]

@; TODO Need ability to override or extend default typesetting.
@;@defproc[(interp-paren-x64-v1 [v paren-x64-v1?]) (in-range/c 0 256)]{
@;The reference interpreter for @racket[paren-x64-v1]; produces the final value of
@;@tt{rax} as an exit code (modulo 256).
@;
@;@examples[#:eval egv1
@;(interp-paren-x64-v1 `(begin (set! rax 42)))
@;]
@;}
