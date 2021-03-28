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
 "../../langs/a6.rkt")

(provide (all-defined-out))

(define values-lang-v6-programs
  '(
    (module
      (define id
        (lambda (x)
          x))
      (call id 5))

    (module
      (define id
        (lambda (x)
          x))
      (let ([y (call id 5)])
        (+ 5 y)))

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
        (call y 5)))

    (module
      (define fact
        (lambda (x)
          (if (= x 0)
              1
              (let ([z (+ x -1)])
                (let ([y (call fact z)])
                  (* x y))))))
      (call fact 5))))

(define (a6-end-to-end-test-suite passes)
  (test-suite
   "a6 end-to-end"
   (for/list ([i values-lang-v6-programs])
     (test-correct interp-values-lang-v6 execute i i))))

(require "a6-progs.rkt")

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
         assign-frame-variables
         replace-locations
         implement-fvars
         optimize-predicates
         expose-basic-blocks
         resolve-predicates
         flatten-program
         patch-instructions
         generate-x64)

  (define decoder (current-actual-decoder))
  (define masker (current-expected-masker))
  (define encoder (current-input-encoder))
  (define run/read (current-run/read))

  (test-suite
   "a6 public test suite"

   #:before
   (thunk
    (current-run/read nasm-run/print-number))

   #:after
   (thunk
    (current-run/read run/read))

   (make-tests
    "a6"

    (reverse
     (list
      sequentialize-let-source-progs
      impose-calling-conventions-source-progs
      canonicalize-bind-source-progs
      select-instructions-source-progs
      uncover-locals-source-progs
      undead-analysis-source-progs
      conflict-analysis-source-progs
      assign-call-undead-variables-source-progs
      allocate-frames-source-progs
      assign-registers-source-progs
      assign-frame-variables-source-progs
      replace-locations-source-progs
      optimize-predicates-source-progs
      implement-fvars-source-progs
      expose-basic-blocks-source-progs
      resolve-predicates-source-progs
      flatten-program-source-progs
      patch-instructions-source-progs
      generate-x64-source-progs))

    (map interp-values-lang-v6 values-lang-v6-programs)

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
    canonicalize-bind
    impose-calling-conventions
    sequentialize-let)

   (a6-end-to-end-test-suite passes)))
