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
 racket/performance-hint
 racket/engine
 racket/sandbox
 ;"../../langs/a9.rkt"
 "a10.rkt"
 "a11-front-end.rkt"
 #;"a8.rkt")

(provide (all-defined-out))

(define (a11-public-test-suite)

  (define decoder (current-actual-decoder))
  (define masker (current-expected-masker))
  (define encoder (current-input-encoder))
  (define run/read (current-run/read))
  (define passls (current-pass-list))

  (test-suite
   "a11 public test suite"

   #:before
   (thunk
    (current-pass-list
     (list*
      expand-macros
      applyify
      (current-pass-list)))
    (current-run/read nasm-run/read))

   #:after
   (thunk
    (current-pass-list passls)
    (current-run/read run/read))))
