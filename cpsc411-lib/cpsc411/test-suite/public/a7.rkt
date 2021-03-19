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
 "../../langs/a7.rkt"
 )

(provide (all-defined-out))

(define exprs-lang-v7-programs
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
          (call + 5 y)))

    (module
        (define odd?
          (lambda (x)
            (if (call eq? x 0)
                0
                (let ([y (call + x -1)])
                  (call even? y)))))
        (define even?
          (lambda (x)
            (if (call eq? x 0)
                1
                (let ([y (call + x -1)])
                  (call odd? y)))))
      (call even? 5))

    (module (define zero (lambda (v0 v1 v2 v3) 0)) 0)

    (module (define id (lambda (x) x)) (let ([y id]) (call y 5)))

    (module
      (define id1 (lambda (x) x))
      (define id2 (lambda (x) x))
      (let ([y (if #t id1 id2)])
        (call y 5)))

    (module (call + (call + 5 6) (call * 4 5)))
    (module (if (call eq? (call + 5 6) 11) 4 6))
    (module #t)
    (module #f)
    (module empty)
    #;(module (void))

    (module
      (define F
        (lambda (a b c d e f g)
          (call + 10 (call G a b c d e f g 8))))
      (define G
        (lambda (a b c d e f g h)
          (call H a b c d e f g h 9)))
      (define H
        (lambda (a b c d e f g h j)
          (let ([r1 (call + a b)])
            (let ([r2 (call + r1 c)])
              (let ([r3 (call + r2 d)])
                (let ([r4 (call + r3 e)])
                  (let ([r5 (call + r4 f)])
                    (let ([r6 (call + r5 g)])
                      (let ([r7 (call + r6 h)])
                        (call + r7 j))))))))))
      (call F 1 2 3 4 5 6 7))

    (module
      (define swap
        (lambda (x y)
          (if (call < y x)
              x
              (call swap y x))))
      (call swap 1 2))

    (module
      (define fact_loop
        (lambda (n acc)
          (if (call eq? n 0)
              acc
              (call fact_loop (call - n 1) (call * acc n)))))
      (call fact_loop 5 1))

    (module
      (define fact
        (lambda (x)
          (if (call eq? x 0)
              1
              (call * x (call fact (call - x 1))))))
      (call fact 5))

    (module
      (define fib_loop
        (lambda (n acc1 acc2)
          (if (call eq? n 0)
              acc1
              (if (call eq? n 1)
                  acc2
                  (let ([new-n (call + n -1)])
                    (let ([new-acc2 (call + acc1 acc2)])
                      (call fib_loop new-n acc2 new-acc2)))))))
      (call fib_loop 5 0 1))

    (module
      (define fib_loop
        (lambda (n acc1 acc2)
          (if (call eq? n 0)
              acc1
              (if (call eq? n 1)
                  acc2
                  (let ([new-n (call + n -1)])
                    (let ([new-acc2 (call + acc1 acc2)])
                      (call fib_loop new-n acc2 new-acc2)))))))
      (call fib_loop 5 0 1))
    ))


(define (a7-end-to-end-test-suite passes)
  (test-suite
   "a7 end-to-end"
   (for/list ([i exprs-lang-v7-programs])
     (test-correct interp-exprs-lang-v7 execute i i))))

(define (a7-void-test-suite passes)
  (test-suite
   "a7 void tests"
   (test-equal? "" (execute '(module (void)) nasm-run/print-string) "")))

(define (a7-public-test-suite
         passes

         interp-paren-x64

         check-exprs-lang
         uniquify
         implement-safe-primops
         specify-representation
         remove-complex-opera*
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
         optimize-predicates
         implement-fvars
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
   "a7 public test suite"

   #:before
   (thunk
    (current-run/read nasm-run/read))

   #:after
   (thunk
    (current-run/read run/read))

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
    canonicalize-bind
    impose-calling-conventions
    sequentialize-let)

   (a7-end-to-end-test-suite passes)
   (a7-void-test-suite passes)))
