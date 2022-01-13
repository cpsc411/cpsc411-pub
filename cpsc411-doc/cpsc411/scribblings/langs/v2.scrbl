#lang scribble/manual

@(require
  "utils.rkt"
  cpsc411/langs/v2
  (for-label cpsc411/langs/v2)
  (for-label racket/contract))

@title{v2 Languages}
@defmodule[cpsc411/langs/v2]

@deflangs[
asm-lang-v2
asm-lang-v2/locals
asm-lang-v2/assignments
nested-asm-lang-v2
para-asm-lang-v2
paren-x64-fvars-v2
paren-x64-v2
]
