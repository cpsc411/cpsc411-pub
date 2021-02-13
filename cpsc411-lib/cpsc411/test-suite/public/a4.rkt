#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib
 "../../langs/a4.rkt"
 "a2.rkt"
 "a3.rkt")

(provide (all-defined-out))


(define (a4-expose-basic-blocks-test-suite passes expose-basic-blocks)
  (define (check-ebb-correct actual source)
    (check-correct interp-nested-asm-lang-v4 interp-block-asm-lang-v4 source actual))

  (test-suite
   "a4 expose-basic-blocks tests"

   (let ([x `(module (begin (halt 5)))])
     (fragile-test-case
      (check-match (expose-basic-blocks x)
                   `(module (define ,L.main.1 (begin (halt 5))))))

     (check-ebb-correct (expose-basic-blocks x) x)

     (check-from expose-basic-blocks passes x 5))

   (let ([x `(module (begin (begin (halt 5))))])
     (fragile-test-case
      (check-match (expose-basic-blocks x)
                   `(module (define ,L.main.1 (begin (halt 5))))))

     (check-ebb-correct (expose-basic-blocks x) x)

     (check-from expose-basic-blocks passes x 5))

   (let ([x `(module
               (begin (begin (set! fv0 1)
                             (set! fv1 2))
                      (set! fv0 (+ fv0 fv1))
                      (halt fv0)))])
     (check-match (expose-basic-blocks x)
                  `(module
                     (define ,L.main.1
                       (begin
                         (set! fv0 1)
                         (set! fv1 2)
                         (set! fv0 (+ fv0 fv1))
                         (halt fv0)))))
     (check-ebb-correct (expose-basic-blocks x) x)

     (check-from expose-basic-blocks passes x 3))

   (let ([x `(module
               (begin (begin (set! fv0 1)
                             (set! fv1 2))
                      (begin (begin (set! fv0 (+ fv0 fv1))))
                      (halt fv0)))])
     (check-match (expose-basic-blocks x)
                  `(module
                     (define ,L.main.1
                       (begin
                         (set! fv0 1)
                         (set! fv1 2)
                         (set! fv0 (+ fv0 fv1))
                         (halt fv0)))))

     (check-ebb-correct (expose-basic-blocks x) x)

     (check-from expose-basic-blocks passes x 3))))

(define (a4-public-test-suite
         passes
         link-paren-x64
         interp-paren-x64
         interp-values-lang

         uniquify
         sequentialize-let
         canonicalize-bind
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
         generate-x64
         wrap-x64-run-time
         wrap-x64-boilerplate)

  (define decoder (current-actual-decoder))
  (define masker (current-expected-masker))
  (define encoder (current-input-encoder))
  (define run/read (current-run/read))

  (test-suite
   "a4 public test suite"
   #:before
   (thunk
    (current-run/read nasm-run/print-number))
   #:after
   (thunk
    (current-run/read run/read))

   (test-suite
    "a4 backwards compatibility tests"
    (a2-paren-x64-v2-test-suite (list
                                 values; check-paren-x64
                                 generate-x64
                                 wrap-x64-run-time
                                 wrap-x64-boilerplate)
                                interp-paren-x64)

    (a2-implement-fvars-test-suite passes implement-fvars)
    (a2-patch-instructions-test-suite passes patch-instructions)
    #;(a2-flatten-begins-test-suite passes flatten-program)
    (a2-select-instructions-test-suite passes select-instructions)
    #;(a2-assign-homes-test-suite passes assign-homes-opt)
    #;(a2-assign-homes-test-suite passes assign-homes)
    (a2-canonicalize-bind-test-suite passes canonicalize-bind)
    (a2-sequentialize-let-test-suite passes sequentialize-let)
    (a2-uniquify-test-suite passes uniquify)
    #;(a2-check-values-lang-test-suite passes check-values-lang)
    (a2-values-lang-test-suite passes interp-values-lang)

    (a3-public-test-undead-sets undead-analysis)
    (a3-conflict-analysis-tests conflict-analysis)
    (a3-assign-registers-stress-tests assign-registers))

   (a4-expose-basic-blocks-test-suite passes expose-basic-blocks)))

;; Local Variables:
;; eval: (put 'module 'racket-indent-function 0)
;; End:
