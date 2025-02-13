#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 racket/contract
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

(define (map-add-test ls)
  `(module
     (define map
       (lambda (f ls)
         (if (call eq? empty ls)
             empty
             (call cons (call f (call car ls))
                   (call map f (call cdr ls))))))
     (call map (lambda (x) (call + 1 x)) ,ls)))

(register-test-programs!
 interp-exprs-lang-v9
 `(("map singleton"
    ,(map-add-test `(call cons 1 empty)))

   ("map ls 3"
    ,(map-add-test `(call cons 1 (call cons 2 (call cons 3 empty)))))

   (""
    (module
      (define swap
        (lambda (x y)
          (if (call < y x)
              x
              (let ([z (call swap y x)])
                z))))
      (call swap 1 2)))))

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
    (compiler-testomatic pass-ls interp-ls nasm-run/read))

   (test-equal?
    ""
    (execute '(module (lambda (x) x)) nasm-run/print-string)
    "#<procedure>")

   (test-equal?
    ""
    (execute '(module (void)) nasm-run/print-string)
    "")

   (test-pred
     ""
     (and/c uint8? (not/c zero?))
     (execute '(module (call 5 (lambda (x) x))) nasm-run/exit-code))

   (test-pred
     ""
     (and/c uint8? (not/c zero?))
     (execute '(module (call (lambda (x) x) 5 6)) nasm-run/exit-code))))
