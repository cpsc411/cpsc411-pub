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
 "../../langs/a5.rkt"
 "a2.rkt"
 "a3.rkt"
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

(define-syntax (make-tests stx)
  (syntax-case stx ()
    [(_ passes pass ...)
     (with-syntax ([(progs-id ...) (map (curry format-id stx "~a-source-progs")
                                        (syntax->list #'(pass ...)))])
       #`(test-suite
          ""
          (test-suite
           (format "a5 from ~a tests" 'pass)
           (for/list ([t progs-id]
                      [s values-lang-v5-programs])
             (test-from pass passes t (interp-values-lang-v5 s))))
          ...))]))

(define (a5-public-test-suite
         passes

         ;interp-values-lang
         link-paren-x64
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
    (a2-check-values-lang-test-suite passes check-values-lang)
    #;(a2-values-lang-test-suite passes interp-values-lang)

    (a3-public-test-undead-sets undead-analysis)
    (a3-conflict-analysis-tests conflict-analysis)
    (a3-assign-registers-stress-tests assign-registers)

    (a4-link-paren-x64-test-suite link-paren-x64)
    (a4-interp-paren-x64-test-suite interp-paren-x64)
    (a4-generate-x64-test-suite passes interp-paren-x64 generate-x64)
    (a4-patch-instructions-test-suite passes patch-instructions)
    (a4-flatten-program-test-suite passes flatten-program)
    (a4-resolve-predicates-test-suite passes resolve-predicates)
    (a4-expose-basic-blocks-test-suite passes expose-basic-blocks))

   (make-tests
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
    canonicalize-bind
    impose-calling-conventions
    sequentialize-let)

   #;(a5-end-to-end-test-suite passes)
   ))
