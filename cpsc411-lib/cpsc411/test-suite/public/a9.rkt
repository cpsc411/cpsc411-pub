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
 ;"../../langs/a9.rkt"
 "a8.rkt"
 )

(provide (all-defined-out))

(define (map-add-test ls)
  `(module
     (define map
       (lambda (f ls)
         (if (call eq? empty ls)
             empty
             (call cons (call f (call car ls))
                   (call map f (call cdr ls))))))
     (call map (lambda (x) (call + 1 x)) ,ls)))

(define (a9-public-test-suite
         passes

         check-exprs-lang
         uniquify
         implement-safe-primops
         implement-safe-call
         define->letrec
         optimize-direct-calls
         dox-lambdas
         uncover-free
         convert-closures
         optimize-known-calls
         hoist-lambdas
         implement-closures
         specify-representation
         remove-complex-opera*
         sequentialize-let
         impose-calling-conventions
         normalize-bind
         select-instructions
         expose-allocation-pointer
         uncover-locals
         undead-analysis
         conflict-analysis
         assign-call-undead-variables
         allocate-frames
         assign-registers
         assign-frame-variables
         replace-locations
         optimize-predicates
         implement-fvars
         expose-basic-blocks
         resolve-predicates
         flatten-program
         patch-instructions
         implement-mops
         generate-x64)

  (define decoder (current-actual-decoder))
  (define masker (current-expected-masker))
  (define encoder (current-input-encoder))
  (define run/read (current-run/read))

  (test-suite
   "a9 public test suite"

   #:before
   (thunk
    (current-run/read nasm-run/read))

   #:after
   (thunk
    (current-run/read run/read))

   (test-suite
   "a8 backwards compatibility tests"

   (a8-end-to-end-test-suite))

   (test-equal?
    ""
    (execute '(module (lambda (x) x)) nasm-run/print-string)
    "#<procedure>")

   (test-equal?
    ""
    (execute '(module
                (define swap
                  (lambda (x y)
                    (if (call < y x)
                        x
                        (let ([z (call swap y x)])
                          z))))
                (call swap 1 2)))
    2)

   (test-equal?
    ""
    (execute
     (map-add-test `(call cons 1 empty)))
    '(2))

   (test-equal?
    ""
    (execute
     (map-add-test `(call cons 1 (call cons 2 (call cons 3 empty)))))
    '(2 3 4))

   #;(make-tests
      passes
      generate-x64
      patch-instructions
      flatten-program
      resolve-predicates
      expose-basic-blocks
      optimize-predicates
      implement-fvars
      assign-frame-variables
      replace-locations
      assign-registers
      allocate-frames
      assign-call-undead-variables
      conflict-analysis
      undead-analysis
      uncover-locals
      select-instructions
      normalize-bind
      impose-calling-conventions
      sequentialize-let
      remove-complex-opera*
      specify-representation
      implement-safe-primops)

   #;(a8-end-to-end-test-suite passes)
   #;(a8-void-test-suite passes)
   ))
