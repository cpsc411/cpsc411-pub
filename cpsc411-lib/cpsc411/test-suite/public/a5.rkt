#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 (for-syntax
  racket/function
  racket/syntax)
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 "../../langs/v5.rkt"
 "a4.rkt")

(provide (all-defined-out))

(define values-lang-v5-programs
  '(
    (module
      (define id
        (lambda (x)
          x))
      (call id 5))

    (module
      (define odd?
        (lambda (x)
          (if (= x 0)
              0
              (let ([y (+ x -1)])
                (call even? y)))))
      (define even?
        (lambda (x)
          (if (= x 0)
              1
              (let ([y (+ x -1)])
                (call odd? y)))))
      (call even? 5))

    (module (define zero (lambda (v0 v1 v2 v3) 0)) 0)

    (module (define id (lambda (x) x)) (let ([y id]) (call y 5)))

    (module
      (define id1 (lambda (x) x))
      (define id2 (lambda (x) x))
      (let ([y (if (true) id1 id2)])
        (call y 5)))))


(require "a5-progs.rkt")

(define (a5-end-to-end-test-suite passes)
  (test-suite
   "a5 end-to-end"
   (for/list ([i values-lang-v5-programs])
    (test-correct interp-values-lang-v5 execute i i))))

(define (a5-public-test-suite
         passes

         ;interp-values-lang
         link-paren-x64
         interp-paren-x64

         check-values-lang
         uniquify
         sequentialize-let
         impose-calling-conventions
         normalize-bind
         select-instructions
         uncover-locals
         undead-analysis
         conflict-analysis
         assign-registers
         replace-locations
         assign-homes-opt
         optimize-predicates
         expose-basic-blocks
         resolve-predicates
         flatten-program
         patch-instructions
         implement-fvars
         generate-x64)

  (define decoder (current-actual-decoder))
  (define masker (current-expected-masker))
  (define encoder (current-input-encoder))
  (define run/read (current-run/read))

  (test-suite
   "a5 public test suite"
   #:before
   (thunk
    (current-run/read nasm-run/print-number))
   #:after
   (thunk
    (current-run/read run/read))

   (test-suite
    "a5 backwards compatibility tests"

    (a4-link-paren-x64-test-suite link-paren-x64)
    (a4-interp-paren-x64-test-suite interp-paren-x64)
    (a4-generate-x64-test-suite passes interp-paren-x64 generate-x64)
    (a4-patch-instructions-test-suite passes patch-instructions)
    (a4-flatten-program-test-suite passes flatten-program)
    (a4-resolve-predicates-test-suite passes resolve-predicates)
    (a4-expose-basic-blocks-test-suite passes expose-basic-blocks))

   (make-tests
    "a5"
    (reverse
     (list
      sequentialize-let-source-progs
      normalize-bind-source-progs
      impose-calling-conventions-source-progs
      select-instructions-source-progs
      uncover-locals-source-progs
      undead-analysis-source-progs
      conflict-analysis-source-progs
      assign-registers-source-progs
      replace-locations-source-progs
      optimize-predicates-source-progs
      expose-basic-blocks-source-progs
      resolve-predicates-source-progs
      flatten-program-source-progs
      patch-instructions-source-progs
      implement-fvars-source-progs
      generate-x64-source-progs))

    (map interp-values-lang-v5 values-lang-v5-programs)

    passes
    generate-x64
    implement-fvars
    patch-instructions
    flatten-program
    resolve-predicates
    expose-basic-blocks
    optimize-predicates
    replace-locations
    assign-registers
    conflict-analysis
    undead-analysis
    uncover-locals
    select-instructions
    impose-calling-conventions
    normalize-bind
    sequentialize-let)

   (a5-end-to-end-test-suite passes)))
