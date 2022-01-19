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
  `((""
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
     (module (let ([x 10])
               (let ([y 12])
                 (+ x y)))))
    (""
     (module (let ([foo 1]
                   [bar 2])
               (+ foo bar))))
    (""
     (module (let ([foo 1])
               (let ([bar foo])
                 bar))))
    (""
     (module (let ([foo (let ([bar 1]) bar)]
                   [bar 2])
               (+ foo bar))))

    (""
     (module (let ([foo 1])
               (let ([foo 2])
                 foo))))
    (""
     (module (let ([foo 1])
               (let ([foo (+ 1 foo)])
                 foo))))
    (""
     (module (let ([foo 1])
               (let ([foo (+ 1 foo)]
                     [bar (+ 2 foo)])
                 (+ foo bar)))))
    (""
     (module (let ([x 3]
                   [y 5]
                   [z (let ([q (* 4 2)]
                            [r (+ 5 6)])
                        (+ q r))])
               (let ([m (+ x y)])
                 (* m z)))))
    (""
     (module (let ([x 1]
                   [y (let ([z 3]) z)]
                   [z (let ([y 2]) (+ y y))])
               (+ y z))))
    (""
     (module
       (let ([v 1])
         (let ([w 46])
           (let ([x v])
             (let ([x (+ x 7)])
               (let ([y x])
                 (let ([y (+ y 4)])
                   (let ([z x])
                     (let ([z (+ z w)])
                       (let ([t y])
                         (let ([t (* t -1)])
                           (let ([z (+ z t)])
                             z)))))))))))))))

(hash-set! test-prog-dict interp-values-lang-v3
           (list->mutable-set values-lang-v3-progs))

(define values-unique-lang-v3-progs
  `((""
     (module 5))
    (""
     (module (let ([foo.1 1]) foo.1)))
    (""
     (module (let ([foo.1 1]) (+ foo.1 foo.1))))
    (""
     (module (let ([foo.1 1] [bar.1 2]) (+ foo.1 bar.1))))
    (""
     (module (let ([foo.1 1])
               (let ([bar.1 foo.1])
                 (+ foo.1 bar.1)))))
    (""
     (module (let ([foo.1 1])
               (let ([bar.1 2])
                 (+ foo.1 bar.1)))))
    (""
     (module (let ([bar.1 1])
               (let ([foo.1 2])
                 (+ foo.1 bar.1)))))
    (""
     (module (let ([x.1 5]
                   [y.1 (let ([z.1 (+ 7 8)]) z.1)])
               (+ x.1 y.1))))))

(hash-set! test-prog-dict interp-values-unique-lang-v3
           (list->mutable-set values-unique-lang-v3-progs))

(define imp-mf-lang-v3-progs
  `((""
     (module (begin (set! x.1 (+ 5 6))
                    (set! y.1 (+ 1 x.1))
                    y.1)))
    (""
     (module (begin (set! x.1 (begin (set! y.1 1)
                                     y.1))
                    x.1)))
    (""
     (module
       (begin
         (set! x.3 (begin (set! y.4 (begin (set! z.4 (+ 4 5))
                                           z.4))
                          y.4))
         x.3)))
    (""
     (module
       (begin
         (set! x.6 2)
         (set! x.5 1)
         (begin
           (set! x.4 x.5)
           (set! x.6
                 (begin
                   (set! x.4 2) x.4))
           2))))
    (""
     (module
       (begin
         (set! x.6 (+ 2 3))
         (set! x.7 (+ x.6 x.6))
         (begin
           (set! y.2 5)
           x.6))))
    (""
     (module
       (begin
         (begin
           (set! t.7 (begin (set! r.5 12)
                            r.5))
           (set! p.8 t.7))
         (set! x.3 (begin (set! y.4 (begin (set! z.4 (+ 4 5))
                                           z.4))
                          y.4))
         x.3)))))

(hash-set! test-prog-dict interp-imp-mf-lang-v3
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
                    (set! y.1 1)
                    (set! z.1 (+ x.1 y.1))
                    (set! w.1 x.1)
                    (set! w.1 (+ w.1 z.1))
                    w.1)))
    (""
     (module (begin (set! x.1 1)
                    (set! y.1 (+ 1 x.1))
                    y.1)))
    (""
     (module (begin (+ 1 2))))
    #;(""
       (module undefined.1))))

(hash-set! test-prog-dict interp-imp-cmf-lang-v3
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
