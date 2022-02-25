#lang scribble/manual

@(require
  (for-label
   rackunit
   (except-in cpsc411/compiler-lib compile)
   (except-in racket/base read read-syntax)
   cpsc411/test-suite/public/v2
   cpsc411/test-suite/public/v2-reg-alloc
   cpsc411/langs/v2-reg-alloc
   racket/contract)
  cpsc411/langs/v2-reg-alloc
  scribble/examples)

@title{Version 2 Register Allocation Test Suites}
@defmodule[cpsc411/test-suite/public/v2-reg-alloc]
@(require (for-label cpsc411/langs/v2-reg-alloc))

@defproc[(v2-reg-alloc-public-test-suite [undead-analysis (-> asm-lang-v2/locals? asm-lang-v2/undead?)]
                                         [conflict-analysis (-> asm-lang-v2/undead? asm-lang-v2/conflicts?)]
                                         [assign-registers (-> asm-lang-v2/conflicts? asm-lang-v2/assignments?)])
         test-suite?]{

The test suite for the v2 register allocation compiler passes.

Expects the 3 function specifically related to register allocation, and performs
various kinds of property-based testing on them.
Checks that:
@itemlist[
@item{@racket[undead-analysis] produces expected undead sets, including
properly ignoring set order.}
@item{@racket[conflict-analysis] produces expected conflict graphs, including
properly ignoring order.}
@item{@racket[assign-registers] produces valid assignments, i.e., assignments
are never assigned to conflicting variables.}
@item{All passes do not transform the program and only affect the info fields}
]
}
