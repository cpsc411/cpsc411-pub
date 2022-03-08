#lang scribble/manual

@(require
  "utils.rkt"
  cpsc411/langs/v7
  (for-label cpsc411/langs/v7)
  (for-label racket/contract))

@title{v7 Languages}
@declare-exporting[cpsc411/langs/v7]

@deflangs[
exprs-lang-v7
exprs-unique-lang-v7
exprs-unsafe-data-lang-v7
exprs-bits-lang-v7
exprs-bits-lang-v7/contexts
values-bits-lang-v7
imp-mf-lang-v7
proc-imp-cmf-lang-v7
imp-cmf-lang-v7
asm-pred-lang-v7
asm-pred-lang-v7/locals
asm-pred-lang-v7/undead
asm-pred-lang-v7/conflicts
asm-pred-lang-v7/pre-framed
asm-pred-lang-v7/framed
asm-pred-lang-v7/spilled
asm-pred-lang-v7/assignments
nested-asm-lang-fvars-v7
nested-asm-lang-v7
block-pred-lang-v7
block-asm-lang-v7
para-asm-lang-v7
paren-x64-v7
paren-x64-rt-v7
]
