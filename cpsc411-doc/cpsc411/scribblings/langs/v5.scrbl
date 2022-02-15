#lang scribble/manual

@(require
  "utils.rkt"
  cpsc411/langs/v5
  (for-label cpsc411/langs/v5)
  (for-label racket/contract))

@title{v5 Languages}
@defmodule[cpsc411/langs/v5]

@deflangs[
values-lang-v5
values-unique-lang-v5
imp-mf-lang-v5
proc-imp-cmf-lang-v5
imp-cmf-lang-v5
asm-pred-lang-v5
asm-pred-lang-v5/locals
asm-pred-lang-v5/undead
asm-pred-lang-v5/conflicts
asm-pred-lang-v5/assignments
nested-asm-lang-v5
block-pred-lang-v5

block-asm-lang-v5
para-asm-lang-v5
paren-x64-fvars-v5
paren-x64-v5
]
