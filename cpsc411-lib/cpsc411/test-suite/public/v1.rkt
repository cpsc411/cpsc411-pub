#lang reader "../../test-lang/lang/reader.rkt"

(require
  racket/set
  racket/match
  racket/function
  racket/list
  rackunit
  "../utils.rkt"
  "../../langs/v1.rkt"
  cpsc411/compiler-lib)

(provide
 (all-defined-out))

(define paren-x64-v1-progs
  ;; string? x paren-x64-v1?
  ;; a name and a test program
  ;; valued define by langs/v1 interpreter
  `((""
     (begin (set! rax 5)))

    (""
     (begin
       (set! rdi 5)
       (set! rax rdi)))

    (""
     (begin
       (set! rax 4)
       (set! rax (+ rax 1))))

   ))

(register-test-programs!
 interp-paren-x64-v1
 (list->mutable-set paren-x64-v1-progs))

(define (v1-check-paren-x64-syntax-public-test-suite check-paren-x64)
  (test-suite
   "milestone-1 check-syntax public tests"

   (test-suite
    "test that bad syntax is rejected"

    (let ([x `(set! rax 5)])
      (test-validator-exn "must have a begin" check-paren-x64 x))

    (let ([x `(begin (set! rax "not a number!!!"))])
      (test-validator-exn "rejects patent nonsense" check-paren-x64 x))

    (let ([x `(begin
                (set! 5 4)
                (set! rax 0))])
      (test-validator-exn "set! must set a reg" check-paren-x64 x))

    )))

(define (v1-check-paren-x64-initialization-public-test-suite check-paren-x64)
  (test-suite
   "milestone-1 check initialization public tests"

   (let ([x `(begin (set! rax rdi))])
     (test-validator-exn "in plain set!" check-paren-x64 x))

   (let ([x `(begin (set! rax (+ rax 10)))])
     (test-validator-exn "in add" check-paren-x64 x))
   ))

(define (v1-interp-paren-x64-test-suite interp-paren-x64)
  (define decoder (current-actual-decoder))
  (define masker (current-expected-masker))
  (define encoder (current-input-encoder))
  (define run/read (current-run/read))

  (test-suite
   "milestone-1 interp-paren-x64 tests"
   #:before
   (thunk
    (current-run/read nasm-run/exit-code)
    (current-expected-masker exit-code-mask))
   #:after
   (thunk
    (current-run/read run/read)
    (current-expected-masker masker))

   (for/list ([i paren-x64-v1-progs])
     (test-case (first i)
       (check-equal?/upto (interp-paren-x64 (second i))
                          (interp-paren-x64-v1 (second i)))))))

(define (v1-public-test-suite passes interp-list
                              check-paren-x64 interp-paren-x64)
  (define decoder (current-actual-decoder))
  (define masker (current-expected-masker))
  (define encoder (current-input-encoder))
  (define run/read (current-run/read))

  (test-suite
   "milestone-1 public test suite"
   #:before
   (thunk
    (current-run/read nasm-run/exit-code)
    (current-expected-masker exit-code-mask))
   #:after
   (thunk
    (current-run/read run/read)
    (current-expected-masker masker))

   (v1-interp-paren-x64-test-suite interp-paren-x64)

   (test-suite
    "check-paren-x64 tests"

    (v1-check-paren-x64-syntax-public-test-suite check-paren-x64)
    (v1-check-paren-x64-initialization-public-test-suite check-paren-x64))

   (test-suite
    "testomatic test suite"
    (compiler-testomatic passes interp-list))))
