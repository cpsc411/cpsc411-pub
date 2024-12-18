#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/set
 racket/match
 racket/function
 racket/list
 rackunit
 "../utils.rkt"
 "../../langs/v2.rkt"
 "../../langs/v3.rkt"
 "v1.rkt"
 "v2.rkt"
 cpsc411/compiler-lib)

(provide
 (all-defined-out))

(define values-lang-v3-progs
  ;; string? x values-lang-v3?
  ;; a name and a test program
  `(
    (""
     (module 5))

    (""
     (module (+ 1 2)))

    (""
     (module (* 3 2)))

    (""
     (module (let ([x 10]) x)))

    (""
     (module (let ([foo 1]) foo)))

    (""
     (module (let ([foo 1]) (+ foo foo))))

    (""
     (module (let ([foo 1])
               (let ([bar 2])
                 (+ foo bar)))))

    (""
     (module (let ([foo 1])
               (let ([bar (let ([x 1]) (+ x 5))])
                 (+ foo bar)))))

    ))

(register-test-programs!
 interp-values-lang-v3
 (list->mutable-set values-lang-v3-progs))

(define values-unique-lang-v3-progs
  `(
    (""
     (module 5))

    (""
     (module (let ([foo.1 1]) foo.1)))

    (""
     (module (let ([foo.1 1]) (+ foo.1 foo.1))))

    (""
     (module (let ([foo.1 1])
               (let ([bar.1 foo.1])
                 (+ foo.1 bar.1)))))
    ))

(register-test-programs!
 interp-values-unique-lang-v3
 (list->mutable-set values-unique-lang-v3-progs))

(define imp-mf-lang-v3-progs
  `(
    (""
     (module 5))

    (""
     (module (begin (set! x.1 (+ 5 6))
                    (set! y.1 (+ 1 x.1))
                    y.1)))
    (""
     (module
       (begin
         (set! x.6 (+ 2 3))
         (set! x.7 (+ x.6 x.6))
         (begin
           (set! y.2 5)
           x.6))))
))

(register-test-programs!
 interp-imp-mf-lang-v3
 (list->mutable-set imp-mf-lang-v3-progs))

(define imp-cmf-lang-v3-progs
  `((""
     (module 5))
    (""
     (module (+ 1 2)))
    (""
     (module (begin (set! x.1 1)
                    (set! y.1 1)
                    (+ x.1 y.1))))
    (""
     (module (begin (set! x.1 1) x.1)))
    (""
     (module (begin (set! x.1 1)
                    (set! y.1 1)
                    (set! z.1 (+ x.1 y.1))
                    z.1)))
    (""
     (module (begin (set! x.1 1)
                    (set! y.1 (+ 1 x.1))
                    y.1)))
    ))

(register-test-programs!
 interp-imp-cmf-lang-v3
 (list->mutable-set imp-cmf-lang-v3-progs))

(define (v3-public-test-sutie pass-ls interp-ls)
  (define run/read (current-run/read))

  (test-suite
   "v3 public test suite"
   #:before
   (thunk
    (current-run/read nasm-run/print-number))
   #:after
   (thunk
    (current-run/read run/read))

   (test-suite
    "compiler testomatic test suite"
    (compiler-testomatic pass-ls interp-ls))))
