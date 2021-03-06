#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 (for-syntax
  racket/function
  racket/syntax)
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
; "../../langs/a6.rkt"
 )

(provide (all-defined-out))


(define (a6-public-test-suite
         passes

         interp-paren-x64

         check-values-lang
         uniquify
         sequentialize-let
         impose-calling-conventions
         canonicalize-bind
         select-instructions
         uncover-locals
         undead-analysis
         conflict-analysis
         assign-call-undead-variables
         allocate-frames
         assign-registers
         replace-locations
         assign-frame-variables
         implement-fvars
         optimize-predicates
         expose-basic-blocks
         resolve-predicates
         flatten-program
         patch-instructions
         generate-x64)
  (displayln "Warning: there are no a6 public tests yet")
  (test-suite
   "a6 public test suite"))
