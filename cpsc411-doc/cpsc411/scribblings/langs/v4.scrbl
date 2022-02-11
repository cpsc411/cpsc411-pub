#lang scribble/manual

@(require
  "utils.rkt"
  cpsc411/langs/v4
  (for-label cpsc411/langs/v4)
  (for-label racket/contract))

@title{v4 Languages}
@defmodule[cpsc411/langs/v4]

@deflangs[
values-lang-v4
values-unique-lang-v4
imp-mf-lang-v4
imp-cmf-lang-v4
asm-pred-lang-v4
asm-pred-lang-v4/locals
asm-pred-lang-v4/undead
asm-pred-lang-v4/conflicts
asm-pred-lang-v4/assignments
nested-asm-lang-v4
block-pred-lang-v4
block-asm-lang-v4
para-asm-lang-v4
paren-x64-fvars-v4
paren-x64-rt-v4
paren-x64-v4
]
