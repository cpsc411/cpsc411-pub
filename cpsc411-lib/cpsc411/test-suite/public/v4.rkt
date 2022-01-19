#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib
 racket/dict
 "../../langs/v4.rkt"
 "../../langs/v3.rkt"
 "../../langs/v2.rkt"
 "../utils.rkt"
 "v1.rkt"
 "v2.rkt"
 "v3.rkt")

(provide (all-defined-out))

(for ([(new-v alias-v)
       (in-dict
        `((,interp-values-lang-v4 . ,interp-values-lang-v3)
          (,interp-values-unique-lang-v4 . ,interp-values-unique-lang-v3)
          (,interp-imp-mf-lang-v4 . ,interp-imp-mf-lang-v3)
          (,interp-imp-cmf-lang-v4 . ,interp-imp-cmf-lang-v3)
          (,interp-asm-pred-lang-v4 . ,interp-asm-lang-v2)
          (,interp-asm-pred-lang-v4/locals . ,interp-asm-lang-v2/locals)
          (,interp-asm-pred-lang-v4/assignments . ,interp-asm-lang-v2/assignments)
          ;; needs update
          #;(,interp-nested-asm-lang-v4 . ,interp-nested-asm-lang-v2)
          (,interp-para-asm-lang-v4 . ,interp-para-asm-lang-v2)
          (,interp-paren-x64-fvars-v4 . ,interp-paren-x64-fvars-v2)
          (,interp-paren-x64-v4 . ,interp-paren-x64-v2)))])
  (hash-set! test-prog-dict new-v
             (hash-ref test-prog-dict alias-v)))

(hash-set! test-prog-dict interp-nested-asm-lang-v4
           (list->mutable-set
            (for/list ([p nested-asm-lang-v2-progs])
              `(,(first p) (module ,(second p))))))

(define (v4-public-test-suite pass-ls interp-ls)
  (define run/read (current-run/read))
  (define passes (current-pass-list))

  (test-suite
   "v4 public test suite"
   #:before
   (thunk
    (current-pass-list pass-ls)
    (current-run/read nasm-run/print-number))
   #:after
   (thunk
    (current-pass-list passes)
    (current-run/read run/read))

   (test-suite
    "compiler testomatic test suite"
    (compiler-testomatic pass-ls interp-ls))))
