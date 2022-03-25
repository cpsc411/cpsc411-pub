#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib
 racket/dict
 "../../langs/v9.rkt"
 "../../langs/v8.rkt"
 "../utils.rkt"
 "v1.rkt"
 "v2.rkt"
 "v3.rkt"
 "v4.rkt"
 "v5.rkt"
 "v6.rkt"
 "v7.rkt"
 "v8.rkt")

(provide (all-defined-out))

(for ([(new-v alias-v)
       (in-dict
        `(
          (,interp-exprs-lang-v9 . ,interp-exprs-lang-v8)
          #;(,interp-exprs-unique-lang-v9 . ,interp-exprs-unique-lang-v8)
          (,interp-exprs-unsafe-data-lang-v9 . ,interp-exprs-unsafe-data-lang-v8)))])
  (register-test-programs! new-v (set->list (hash-ref test-prog-dict alias-v '()))))

(define (v9-public-test-suite pass-ls interp-ls)
  (define run/read (current-run/read))
  (define passes (current-pass-list))

  (test-suite
   "v9 public test suite"
   #:before
   (thunk
    (current-pass-list pass-ls)
    (current-run/read nasm-run/read))
   #:after
   (thunk
    (current-pass-list passes)
    (current-run/read run/read))

   (test-suite
    "compiler testomatic test suite"
    (compiler-testomatic pass-ls interp-ls nasm-run/read))))
