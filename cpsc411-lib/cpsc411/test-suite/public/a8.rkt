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
 ;"../../langs/a8.rkt"
 "a7.rkt"
 )

(provide (all-defined-out))

(define (a8-end-to-end-test-suite)
  (test-suite
   "a8 end-to-end"

   (test-equal?
    ""
    (execute
     `(module (call cons 7 empty)))
    '(7))

   (test-equal?
    ""
    (execute
     `(module (call car (call cons 7 empty))))
    7)

   (test-pred
    ""
    (and/c integer? (not/c zero?))
    (execute
     `(module (call car 7))
     nasm-run/exit-code))

   (test-equal?
    ""
    (execute
     `(module (call cons (call * 7 8) empty)))
    '(56))

   (test-equal?
    ""
    (execute
     `(module (call cons (if (call eq? 7 8) (call * 7 8) (call * 8 7)) empty)))
    '(56))

   (test-equal?
    ""
    (execute
     `(module
        (let ([x.1 (call make-vector 0)])
          x.1)))
    #())

   (test-equal?
    ""
    (execute
     `(module
        (let ([x.1 (call make-vector 2)])
          x.1)))
    #(0 0))

   (test-pred
    ""
    (and/c integer? (not/c zero?))
    (execute `(module (call make-vector #\x)) nasm-run/exit-code))

   (test-equal?
    ""
    (execute
     `(module
        (let ([x.1 (call make-vector 3)])
          (let ([x.2 (call vector-set! x.1 0 1)])
            (let ([x.3 (call vector-set! x.1 1 2)])
              (let ([x.4 (call vector-set! x.1 2 3)])
                x.1))))))
    #(1 2 3))

   (check-equal?
    (execute
     `(module
        (let ([x.1 (call make-vector 3)])
          (let ([x.2 (call vector-set! x.1 0 1)])
            (let ([x.3 (call vector-set! x.1 1 2)])
              (let ([x.4 (call vector-set! x.1 2 3)])
                x.1))))))
    #(1 2 3))

   (test-equal?
    ""
    (execute
     `(module
        (let ([x.1 (call make-vector 2)])
          (let ([x.2 (call vector-set! x.1 0 1)])
            (let ([x.3 (call vector-set! x.1 1 2)])
              (let ([x.4 (call vector-set! x.1 2 3)])
                x.1))))))
    #(1 2))

   (test-pred
    ""
    (and/c integer? (not/c zero?))
    (execute
     `(module
        (let ([x.1 (call make-vector 2)])
          (let ([x.2 (call vector-set! x.1 0 1)])
            (let ([x.3 (call vector-set! x.1 1 2)])
              (let ([x.4 (call vector-set! x.1 2 3)])
                x.4)))))
     nasm-run/exit-code))))

(define (a8-public-test-suite
         passes

         check-exprs-lang
         uniquify
         implement-safe-primops
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
   "a7 public test suite"

   #:before
   (thunk
    (current-run/read nasm-run/read))

   #:after
   (thunk
    (current-run/read run/read))

   (a7-public-test-suite
    (current-pass-list)

    values

    check-exprs-lang
    uniquify
    implement-safe-primops
    specify-representation
    remove-complex-opera*
    sequentialize-let
    impose-calling-conventions
    normalize-bind
    select-instructions
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
    generate-x64)

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
   #;(a8-void-test-suite passes)))
