#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib
 racket/dict
 "../../langs/v5.rkt"
 "../../langs/v4.rkt"
 "../../langs/v3.rkt"
 "../../langs/v2.rkt"
 "../utils.rkt"
 "v1.rkt"
 "v2.rkt"
 "v3.rkt"
 "v4.rkt")

(provide (all-defined-out))

(for ([(new-v alias-v)
       (in-dict
        `(
          (,interp-values-lang-v5 . ,interp-values-lang-v4)
          (,interp-values-unique-lang-v5 . ,interp-values-unique-lang-v4)
          (,interp-imp-mf-lang-v5 . ,interp-imp-mf-lang-v4)
          (,interp-imp-cmf-lang-v5 . ,interp-imp-cmf-lang-v4)
          (,interp-proc-imp-cmf-lang-v5 . ,interp-imp-cmf-lang-v4)
          (,interp-asm-pred-lang-v5 . ,interp-asm-pred-lang-v4)
          (,interp-asm-pred-lang-v5/locals . ,interp-asm-pred-lang-v4/locals)
          (,interp-asm-pred-lang-v5/undead . ,interp-asm-pred-lang-v4/undead)
          (,interp-asm-pred-lang-v5/conflicts . ,interp-asm-pred-lang-v4/conflicts)
          (,interp-asm-pred-lang-v5/assignments . ,interp-asm-pred-lang-v4/assignments)

          (,interp-nested-asm-lang-v5 . ,interp-nested-asm-lang-v4)
          (,interp-para-asm-lang-v5 . ,interp-para-asm-lang-v4)
          (,interp-paren-x64-fvars-v5 . ,interp-paren-x64-fvars-v4)
          (,interp-paren-x64-v5 . ,interp-paren-x64-v4)))])
  (register-test-programs!
   new-v
   (set->list (hash-ref test-prog-dict alias-v '()))))

(register-test-programs!
 interp-values-lang-v5
 '((""
    (module
      (define id
        (lambda (x)
          x))
      (call id 5)))

   (""
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
      (call even? 5)))

   (""
    (module (define zero (lambda (v0 v1 v2 v3) 0)) 0))

   (""
    (module (define id (lambda (x) x)) (let ([y id]) (call y 5))))

   (""
    (module
      (define id1 (lambda (x) x))
      (define id2 (lambda (x) x))
      (let ([y (if (true) id1 id2)])
        (call y 5))))))

(define (v5-public-test-suite pass-ls interp-ls
                              check-values-lang)
  (define run/read (current-run/read))
  (define passes (current-pass-list))

  (test-suite
   "v5 public test suite"
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
    (compiler-testomatic pass-ls interp-ls))

   (v4-check-values-lang check-values-lang)))
