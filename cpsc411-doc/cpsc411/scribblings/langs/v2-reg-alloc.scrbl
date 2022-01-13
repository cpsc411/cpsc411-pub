#lang scribble/manual

@(require
  "utils.rkt"
  cpsc411/langs/v2-reg-alloc
  (for-label cpsc411/langs/v2-reg-alloc)
  (for-label racket/contract))

@title{v2 Register Allocation Languages}
@defmodule[cpsc411/langs/v2-reg-alloc]

@deflangs[
asm-lang-v2/undead
asm-lang-v2/conflicts
]
