#lang scribble/manual

@(require
  "utils.rkt"
  cpsc411/langs/v6
  (for-label cpsc411/langs/v6)
  (for-label racket/contract))

@title{v6 Languages}
@defmodule[cpsc411/langs/v6]

@deflangs[
values-lang-v6
values-unique-lang-v6
imp-mf-lang-v6
proc-imp-cmf-lang-v6
imp-cmf-lang-v6
asm-pred-lang-v6
asm-pred-lang-v6/locals
asm-pred-lang-v6/undead
asm-pred-lang-v6/conflicts
asm-pred-lang-v6/pre-framed
asm-pred-lang-v6/framed
asm-pred-lang-v6/spilled
asm-pred-lang-v6/assignments
nested-asm-lang-fvars-v6
nested-asm-lang-v6
block-pred-lang-v6
block-asm-lang-v6
para-asm-lang-v6
paren-x64-v6
paren-x64-rt-v6
]
