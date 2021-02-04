#lang reader "../../test-lang/lang/reader.rkt"

(require
  racket/match
  rackunit
  cpsc411/test-suite/utils
  cpsc411/compiler-lib
  "a1.rkt")

(provide
 (all-defined-out))

(define (a2-check-values-lang-test-suite passes check-values-lang)
  (test-suite
   "a2 check-values-lang tests"

   (test-suite
    "rejects bad syntax"

    (let ([x `5])
      (test-validator-exn "must have a module" check-values-lang x))

    (let ([x `(module +)])
      (test-validator-exn "must be a tail" check-values-lang x))

    (let ([x `(module foo)])
      (test-validator-exn "identifiers must be bound" check-values-lang x))

    (let ([x `(module (let ([foo 1]
                            [bar foo])
                        bar))])
      (test-validator-exn "let is parallel" check-values-lang x))

    (let ([x `(module (let ([foo 1]
                            [foo 2])
                        foo))])
      (test-validator-exn "collisions in a single let" check-values-lang x)))

   (test-suite
    "accepts good syntax"

    (let ([x `(module 5)])
      (check-validator check-values-lang x))

    (let ([x `(module (+ 1 2))])
      (check-validator check-values-lang x))

    (let ([x `(module (let ([foo 1]) foo))])
      (check-validator check-values-lang x))

    (let ([x '(module
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
                                        z))))))))))))])
      (check-validator check-values-lang x)))))

(define (a2-uniquify-test-suite passes uniquify)
  (test-suite
   "a2 uniquify tests"

   (let ([x `(module 5)])
     (check-match (uniquify x)
                  `(module 5))
     (check-from uniquify passes x 5))

   (let ([x `(module (let ([foo 1]) foo))])
     (check-match (uniquify x)
                  `(module (let ([,foo 1]) ,foo)))
     (check-from uniquify passes x 1))

   (let ([x `(module (let ([foo 1]) (+ foo foo)))])
     (check-match (uniquify x)
                  `(module (let ([,foo 1]) (+ ,foo ,foo))))
     (check-from uniquify passes x 2))

   (let ([x `(module (let ([foo 1])
                       (let ([bar 2])
                         (+ foo bar))))])
     (check-match (uniquify x)
                  `(module (let ([,foo 1])
                             (let ([,bar 2])
                               (+ ,foo ,bar))))
                  (unique? (list foo bar)))
     (check-from uniquify passes x 3))

   (let ([x `(module (let ([foo 1]
                           [bar 2])
                       (+ foo bar)))])
     (check-match (uniquify x)
                  `(module (let ([,foo 1]
                                 [,bar 2])
                             (+ ,foo ,bar)))
                  (unique? (list foo bar)))
     (check-from uniquify passes x 3))

   (let ([x `(module (let ([foo 1])
                       (let ([bar foo])
                         bar)))])
     (check-match (uniquify x)
                  `(module (let ([,foo 1])
                             (let ([,bar ,foo])
                               ,bar)))
                  (unique? (list foo bar)))
     (check-from uniquify passes x 1))

   (let ([x `(module (let ([foo (let ([bar 1]) bar)]
                           [bar 2])
                       (+ foo bar)))])
     (check-match (uniquify x)
                  `(module (let ([,foo (let ([,bar.1 1]) ,bar.1)]
                                 [,bar.2 2])
                             (+ ,foo ,bar.2)))
                  (unique? (list foo bar.1 bar.2)))
     (check-from uniquify passes x 3))

   (let ([x `(module (let ([foo 1])
                       (let ([foo 2])
                         foo)))])
     (check-match (uniquify x)
                  `(module (let ([,foo.1 1])
                             (let ([,foo.2 2])
                               ,foo.2)))
                  (unique? (list foo.1 foo.2)))
     (check-from uniquify passes x 2))

   (let ([x `(module (let ([foo 1])
                       (let ([foo (+ 1 foo)])
                         foo)))])
     (check-match (uniquify x)
                  `(module (let ([,foo.1 1])
                             (let ([,foo.2 (+ 1 ,foo.1)])
                               ,foo.2)))
                  (unique? (list foo.1 foo.2)))
     (check-from uniquify passes x 2))

   (let ([x `(module (let ([foo 1])
                       (let ([foo (+ 1 foo)]
                             [bar (+ 2 foo)])
                         (+ foo bar))))])
     (check-match (uniquify x)
                  `(module (let ([,foo.1 1])
                             (let ([,foo.2 (+ 1 ,foo.1)]
                                   [,bar (+ 2 ,foo.1)])
                               (+ ,foo.2 ,bar))))
                  (unique? (list foo.1 foo.2 bar)))
     (check-from uniquify passes x 5))

   ;; large tests
   (let ([x `(module (let ([x 3]
                           [y 5]
                           [z (let ([q (* 4 2)]
                                    [r (+ 5 6)])
                                (+ q r))])
                       (let ([m (+ x y)])
                         (* m z))))])
     (check-match (uniquify x)
                  `(module (let ([,x 3]
                                 [,y 5]
                                 [,z (let ([,q (* 4 2)]
                                           [,r (+ 5 6)])
                                       (+ ,q ,r))])
                             (let ([,m (+ ,x ,y)])
                               (* ,m ,z))))
                  (unique? (list x y z q r m)))
     (check-from uniquify passes x 152))

   (let ([x `(module (let ([x 1]
                           [y (let ([z 3]) z)]
                           [z (let ([y 2]) (+ y y))])
                       (+ y z)))])
     (check-match (uniquify x)
                  `(module (let ([,x 1]
                                 [,y.1 (let ([,z.2 3]) ,z.2)]
                                 [,z.1 (let ([,y.2 2]) (+ ,y.2 ,y.2))])
                             (+ ,y.1 ,z.1)))
                  (unique? (list x y.1 z.1 z.2 y.2)))
     (check-from uniquify passes x 7))))

(define (a2-sequentialize-let-test-suite passes sequentialize-let)
  (test-suite
   "a2 sequentialize-let tests"

   (let ([x `(module 5)])
     (check-match (sequentialize-let x)
                  `(module 5))
     (check-from sequentialize-let passes x 5))

   (let ([x `(module (let ([foo.1 1]) foo.1))])
     (check-match (sequentialize-let x)
                  `(module (begin (set! foo.1 1) foo.1)))
     (check-from sequentialize-let passes x 1))

   (let ([x `(module (let ([foo.1 1]) (+ foo.1 foo.1)))])
     #;(check-match (sequentialize-let x)
                  `(module (begin (set! foo.1 1) (+ foo.1 foo.1))))
     (check-from sequentialize-let passes x 2))

   (let ([x `(module (let ([foo.1 1] [bar.1 2]) (+ foo.1 bar.1)))])
     #;(check-match (sequentialize-let x)
                  `(module (begin ,@(list-no-order `(set! foo.1 1) `(set! bar.1 2))
                                  (+ foo.1 bar.1))))
     (check-from sequentialize-let passes x 3))

   (let ([x `(module (let ([foo.1 1])
                       (let ([bar.1 foo.1])
                         (+ foo.1 bar.1))))])
     #;(check-match (sequentialize-let x)
                  `(module (begin (set! foo.1 1)
                                  (begin (set! bar.1 foo.1)
                                         (+ foo.1 bar.1)))))
     (check-from sequentialize-let passes x 2))

   (let ([x1 `(module (let ([foo.1 1])
                        (let ([bar.1 2])
                          (+ foo.1 bar.1))))]
         [x2 `(module (let ([bar.1 1])
                        (let ([foo.1 2])
                          (+ foo.1 bar.1))))])
     #;(check-match (sequentialize-let x1)
                  `(module (begin (set! foo.1 1)
                                  (begin (set! bar.1 2)
                                         (+ foo.1 bar.1)))))
     #;(check-match (sequentialize-let x2)
                  `(module (begin (set! bar.1 1)
                                  (begin (set! foo.1 2)
                                         (+ foo.1 bar.1)))))
     (check-from sequentialize-let passes x1 3)
     (check-from sequentialize-let passes x2 3))

   (let ([x `(module (let ([x.1 5]
                           [y.1 (let ([z.1 (+ 7 8)]) z.1)])
                       (+ x.1 y.1)))])
     #;(check-match (sequentialize-let x)
                  `(module (begin ,@(list-no-order `(set! x.1 5)
                                                   `(set! y.1 (begin (set! z.1 (+ 7 8)) z.1)))
                                  (+ x.1 y.1))))
     (check-from sequentialize-let passes x 20))))

(define (a2-canonicalize-bind-test-suite passes canonicalize-bind)
  (test-suite
   "a2 canonicalize-bind tests"

   (let ([x `(module 5)])
     (check-match (canonicalize-bind x)
                  `(module 5))
     (check-from canonicalize-bind passes x 5))

   (let ([x `(module (begin (set! x.1 (+ 5 6))
                            (set! y.1 (+ 1 x.1))
                            y.1))])
     (test-case "Fragile test; failure allowed"
       (check-match (canonicalize-bind x)
                    `(module (begin (set! x.1 (+ 5 6))
                                    (set! y.1 (+ 1 x.1))
                                    y.1))))
     (check-from canonicalize-bind passes x 12))

   (let ([x `(module (begin (set! x.1 (begin (set! y.1 1)
                                             y.1))
                            x.1))])
     (test-case "Fragile test; failure allowed"
       (check-match (canonicalize-bind x)
                    `(module (begin (begin (set! y.1 1)
                                           (set! x.1 y.1))
                                    x.1))))
     (check-from canonicalize-bind passes x 1))

   (let ([x `(module
                 (begin
                   (set! x.3 (begin (set! y.4 (begin (set! z.4 (+ 4 5))
                                                     z.4))
                                    y.4))
                   x.3))])
     (test-case "Fragile test; failure allowed"
       (check-match (canonicalize-bind x)
                    `(module
                         (begin
                           (begin
                             (begin
                               (set! z.4 (+ 4 5))
                               (set! y.4 z.4))
                             (set! x.3 y.4))
                           x.3))))
     (check-from canonicalize-bind passes x 9))

   (let ([x `(module
                 (begin
                   (set! x.6 2)
                   (set! x.5 1)
                   (begin
                     (set! x.4 x.5)
                     (set! x.6
                           (begin
                             (set! x.4 2) x.4))
                     2)))])
     (test-case "Fragile test; failure allowed"
       (check-match (canonicalize-bind x)
                    `(module
                         (begin
                           (set! x.6 2)
                           (set! x.5 1)
                           (begin
                             (set! x.4 x.5)
                             (begin
                               (set! x.4 2)
                               (set! x.6 x.4))
                             2)))))
     (check-from canonicalize-bind passes x 2))

   (let ([x `(module
                 (begin
                   (set! x.6 (+ 2 3))
                   (set! x.7 (+ x.6 x.6))
                   (begin
                     (set! y.2 5)
                     x.6)))])
     (check-match (canonicalize-bind x)
                  `(module
                       (begin
                         (set! x.6 (+ 2 3))
                         (set! x.7 (+ x.6 x.6))
                         (begin
                           (set! y.2 5)
                           x.6))))
     (check-from canonicalize-bind passes x 5))

   (let ([x `(module
                 (begin
                   (begin
                     (set! t.7 (begin (set! r.5 12)
                                      r.5))
                     (set! p.8 t.7))
                   (set! x.3 (begin (set! y.4 (begin (set! z.4 (+ 4 5))
                                                     z.4))
                                    y.4))
                   x.3))])
     (test-case "Fragile test; failure allowed"
       (check-match (canonicalize-bind x)
                    `(module
                         (begin
                           (begin
                             (begin (set! r.5 12) (set! t.7 r.5))
                             (set! p.8 t.7))
                           (begin
                             (begin (set! z.4 (+ 4 5)) (set! y.4 z.4))
                             (set! x.3 y.4))
                           x.3)))
       (check-from canonicalize-bind passes x 9)))))

(define (a2-select-instructions-test-suite passes select-instructions)
  (test-suite
   "a2 select-instructions tests"

   (let ([x `(module 5)])
     (check-match (select-instructions x)
                  `(module
                       ()
                       (halt 5)))
     (check-from select-instructions passes x 5))

   (let ([x `(module (+ 1 2))])
     (test-case "Fragile test; allowed to fail"
       (check-match (select-instructions x)
                    `(module
                         ()
                         (begin (set! ,tmp 1)
                                (set! ,tmp (+ ,tmp 2))
                                (halt ,tmp)))
                    (andmap aloc? (list tmp))))
     (check-from select-instructions passes x 3))

   (let ([x `(module (begin (set! x.1 1)
                            (set! y.1 1)
                            (+ x.1 y.1)))])
     (test-case "Fragile test; allowed to fail"
       (check-match (select-instructions x)
                  `(module
                       ()
                     (begin (set! x.1 1)
                            (set! y.1 1)
                            (set! ,tmp x.1)
                            (set! ,tmp (+ ,tmp y.1))
                            (halt ,tmp)))
                  (andmap aloc? (list tmp))))
     (check-from select-instructions passes x 2))

   (let ([x `(module (begin (set! x.1 1) x.1))])
     (check-match (select-instructions x)
                  `(module
                       ()
                     (begin (set! x.1 1)
                            (halt x.1))))
     (check-from select-instructions passes x 1))

   (let ([x `(module (begin (set! x.1 1)
                            (set! y.1 1)
                            (set! z.1 (+ x.1 y.1))
                            z.1))])
     (test-case "Fragile test; allowed to fail"
       (check-match (select-instructions x)
                    `(module
                         ()
                         (begin (set! x.1 1)
                                (set! y.1 1)
                                (set! ,tmp x.1)
                                (set! ,tmp (+ ,tmp y.1))
                                (set! z.1 ,tmp)
                                (halt z.1)))
                    (andmap aloc? (list tmp))))
     (check-from select-instructions passes x 2))

   (let ([x `(module (begin (set! x.1 1)
                            (set! y.1 1)
                            (set! z.1 (+ x.1 y.1))
                            (set! w.1 x.1)
                            (set! w.1 (+ w.1 z.1))
                            w.1))])
     (test-case "Fragile test; allowed to fail"
       (check-match (select-instructions x)
                    `(module
                         ()
                         (begin (set! x.1 1)
                                (set! y.1 1)
                                (set! ,tmp1 x.1)
                                (set! ,tmp1 (+ ,tmp y.1))
                                (set! z.1 ,tmp1)
                                (set! w.1 x.1)
                                (set! ,tmp2 w.1)
                                (set! ,tmp2 (+ ,tmp2 z.1))
                                (set! w.1 ,tmp2)
                                (halt w.1)))
                    (andmap aloc? (list tmp1 tmp2))))
     (check-from select-instructions passes x 3))

   (let ([x `(module (begin (set! x.1 1)
                            (set! y.1 (+ 1 x.1))
                            y.1))])
     (test-case "Fragile test; allowed to fail"
       (check-match (select-instructions x)
                    `(module
                         ()
                         (begin (set! x.1 1)
                                (set! ,tmp 1)
                                (set! ,tmp (+ ,tmp x.1))
                                (set! y.1 ,tmp)
                                (halt y.1)))
                    (andmap aloc? (list tmp))))
     (check-from select-instructions passes x 2))

   (let ([x `(module (begin (+ 1 2)))])
     (test-case "Fragile test; allowed to fail"
       (check-match (select-instructions x)
                    `(module
                         ()
                         (begin (set! ,tmp 1)
                                (set! ,tmp (+ ,tmp 2))
                                (halt ,tmp)))
                    (andmap aloc? (list tmp))))
     (check-from select-instructions passes x 3))

   (let ([x `(module undefined.1)])
     (check-match (select-instructions x)
                  `(module
                       ()
                       (halt undefined.1))))))

(define (a2-uncover-locals-test-suite passes uncover-locals)
  (test-suite
   "a2 uncover-locals tests"

   (let ([x `(module
                 ()
               (halt 5))])
     (check-match (uncover-locals x)
                  `(module
                       ((locals ()))
                       (halt 5)))
     (check-from uncover-locals passes x 5))

   (let ([x `(module
                 ()
               (begin (set! x.1 5)
                      (halt x.1)))])
     (check-match (uncover-locals x)
                  `(module
                       ((locals ,(list-no-order 'x.1)))
                     (begin (set! x.1 5)
                            (halt x.1))))
     (check-from uncover-locals passes x 5))

   (let ([x `(module
                 ()
               (begin (set! x.1 1)
                      (set! y.1 x.1)
                      (set! y.1 (+ y.1 1))
                      (set! z.1 y.1)
                      (set! z.1 (+ z.1 1))
                      (halt z.1)))])
     (check-match (uncover-locals x)
                  `(module
                       ((locals ,(list-no-order 'x.1 'y.1 'z.1)))
                     (begin (set! x.1 1)
                            (set! y.1 x.1)
                            (set! y.1 (+ y.1 1))
                            (set! z.1 y.1)
                            (set! z.1 (+ z.1 1))
                            (halt z.1))))
     (check-from uncover-locals passes x 3))

   (let ([x `(module
                 ()
               (begin (set! x.1 undefined.1)
                      (halt undefined.2)))])
     (check-match (uncover-locals x)
                  `(module
                       ((locals ,(list-no-order 'x.1 'undefined.1 'undefined.2)))
                     (begin (set! x.1 undefined.1)
                            (halt undefined.2)))))

   (let ([x `(module
                 ()
               (begin
                 (set! x.1 0)
                 (set! w.1 0)
                 (set! y.1 x.1)
                 (set! w.1 (+ w.1 x.1))
                 (set! w.1 (+ w.1 y.1))
                 (halt w.1)))])
     (check-match (uncover-locals x)
                  `(module
                       ((locals ,(list-no-order 'w.1 'y.1 'x.1)))
                     (begin
                       (set! x.1 0)
                       (set! w.1 0)
                       (set! y.1 x.1)
                       (set! w.1 (+ w.1 x.1))
                       (set! w.1 (+ w.1 y.1))
                       (halt w.1))))
     (check-from uncover-locals passes x 0))))

(define (a2-assign-fvars-test-suite passes assign-fvars)
  (test-suite
   "a2 assign-fvars tests"

   (let ([x `(module
                 ((locals ()))
               (halt 5))])
     (check-match (assign-fvars x)
                  `(module
                       ,(list-no-order
                         `(locals ())
                         `(assignment ()))
                       (halt 5)))
     (check-from assign-fvars passes x 5))

   (let ([x `(module
                 ((locals (x.1)))
               (begin (set! x.1 5)
                      (halt x.1)))])
     (check-match (assign-fvars x)
                  `(module
                       ,(list-no-order
                        `(locals (x.1))
                        `(assignment ((x.1 ,f1))))
                     (begin (set! x.1 5)
                            (halt x.1)))
                  (and (unique? (list f1))
                       (andmap fvar? (list f1))))
     (check-from assign-fvars passes x 5))

   (let ([x `(module
                 ((locals (x.1 y.1 z.1)))
               (begin (set! x.1 1)
                      (set! y.1 x.1)
                      (set! y.1 (+ y.1 1))
                      (set! z.1 y.1)
                      (set! z.1 (+ z.1 1))
                      (halt z.1)))])
     (check-match (assign-fvars x)
                  `(module
                       ,(list-no-order
                         `(locals (x.1 y.1 z.1))
                         `(assignment ((x.1 ,f1) (y.1 ,f2) (z.1 ,f3))))
                     (begin (set! x.1 1)
                            (set! y.1 x.1)
                            (set! y.1 (+ y.1 1))
                            (set! z.1 y.1)
                            (set! z.1 (+ z.1 1))
                            (halt z.1)))
                  (and (unique? (list f1 f2 f3))
                       (andmap fvar? (list f1 f2 f3))))
     (check-from assign-fvars passes x 3))

   (let ([x `(module
                 ((locals (x.1 undefined.1 undefined.2)))
               (begin (set! x.1 undefined.1)
                      (halt undefined.2)))])
     (check-match (assign-fvars x)
                  `(module
                       ,(list-no-order
                         `(locals (x.1 undefined.1 undefined.2))
                         `(assignment ((x.1 ,f1) (undefined.1 ,f2) (undefined.2 ,f3))))
                     (begin (set! x.1 undefined.1)
                            (halt undefined.2)))
                  (and (unique? (list f1 f2 f3))
                       (andmap fvar? (list f1 f2 f3)))))

   (let ([x `(module
                 ((locals (w.1 y.1 x.1)))
               (begin
                 (set! x.1 0)
                 (set! w.1 0)
                 (set! y.1 x.1)
                 (set! w.1 (+ w.1 x.1))
                 (set! w.1 (+ w.1 y.1))
                 (halt w.1)))])
     (check-match (assign-fvars x)
                  `(module
                       ,(list-no-order
                         `(locals (w.1 y.1 x.1))
                         `(assignment ((w.1 ,f1) (y.1 ,f2) (x.1 ,f3))))
                     (begin
                       (set! x.1 0)
                       (set! w.1 0)
                       (set! y.1 x.1)
                       (set! w.1 (+ w.1 x.1))
                       (set! w.1 (+ w.1 y.1))
                       (halt w.1)))
                  (and (unique? (list f1 f2 f3))
                       (andmap fvar? (list f1 f2 f3))))
     (check-from assign-fvars passes x 0))))

(define (a2-replace-locations-test-suite passes replace-locations)
  (test-suite
   "a2 replace-locations tests"

   (let ([x `(module
                 ((locals ())
                  (assignment ()))
               (halt 5))])
     (check-match (replace-locations x)
                  `(halt 5)))

   (let ([x `(module
                 ((locals (x.1))
                  (assignment ((x.1 fv0))))
               (begin (set! x.1 5)
                      (halt x.1)))])
     (check-match (replace-locations x)
                  `(begin (set! fv0 5)
                          (halt fv0))))

   (let ([x `(module
                 ((locals (x.1 y.1 z.1))
                  (assignment ((x.1 fv0) (y.1 fv1) (z.1 fv2))))
               (begin (set! x.1 1)
                      (set! y.1 x.1)
                      (set! y.1 (+ y.1 1))
                      (set! z.1 y.1)
                      (set! z.1 (+ z.1 1))
                      (halt z.1)))])
     (check-match (replace-locations x)
                  `(begin (set! fv0 1)
                          (set! fv1 fv0)
                          (set! fv1 (+ fv1 1))
                          (set! fv2 fv1)
                          (set! fv2 (+ fv2 1))
                          (halt fv2))))

   (let ([x `(module
                 ((locals (x.1 undefined.1 undefined.2))
                  (assignment ((x.1 fv0) (undefined.1 fv1) (undefined.2 fv2))))
               (begin (set! x.1 undefined.1)
                      (halt undefined.2)))])
     (check-match (replace-locations x)
                  `(begin (set! fv0 fv1)
                          (halt fv2))))

   (let ([x `(module
                 ((locals (w.1 y.1 x.1))
                  (assignment ((w.1 fv0) (y.1 fv1) (x.1 fv2))))
               (begin
                 (set! x.1 0)
                 (set! y.1 x.1)
                 (set! w.1 (+ w.1 x.1))
                 (set! w.1 (+ w.1 y.1))
                 (halt w.1)))])
     (check-match (replace-locations x)
                  `(begin
                     (set! fv2 0)
                     (set! fv1 fv2)
                     (set! fv0 (+ fv0 fv2))
                     (set! fv0 (+ fv0 fv1))
                     (halt fv0))))))

(define (a2-assign-homes-test-suite passes assign-homes)
  (test-suite
   "a2 assign-homes tests"

   (let ([x `(module
                 ()
               (halt 5))])
     (check-match (assign-homes x)
                  `(halt 5)))

   (let ([x `(module
                 ()
               (begin (set! x.1 5)
                      (halt x.1)))])
     (check-match (assign-homes x)
                  `(begin (set! ,fv0 5)
                          (halt ,fv0))
                  (and (unique? (list fv0))
                       (andmap fvar? (list fv0)))))

   (let ([x `(module
                 ()
               (begin (set! x.1 1)
                      (set! y.1 x.1)
                      (set! y.1 (+ y.1 1))
                      (set! z.1 y.1)
                      (set! z.1 (+ z.1 1))
                      (halt z.1)))])
     (check-match (assign-homes x)
                  `(begin (set! ,fv0 1)
                          (set! ,fv1 ,fv0)
                          (set! ,fv1 (+ ,fv1 1))
                          (set! ,fv2 ,fv1)
                          (set! ,fv2 (+ ,fv2 1))
                          (halt ,fv2))
                  (and (unique? (list fv0 fv1 fv2))
                       (andmap fvar? (list fv0 fv1 fv2)))))

   (let ([x `(module
                 ()
               (begin (set! x.1 undefined.1)
                      (halt undefined.2)))])
     (check-match (assign-homes x)
                  `(begin (set! ,fv0 ,fv1)
                          (halt ,fv2))
                  (and (unique? (list fv0 fv1 fv2))
                       (andmap fvar? (list fv0 fv1 fv2)))))

   (let ([x `(module
                 ()
               (begin
                 (set! x.1 0)
                 (set! y.1 x.1)
                 (set! w.1 (+ w.1 x.1))
                 (set! w.1 (+ w.1 y.1))
                 (halt w.1)))])
     (check-match (assign-homes x)
                  `(begin
                     (set! ,fv2 0)
                     (set! ,fv1 ,fv2)
                     (set! ,fv0 (+ ,fv0 ,fv2))
                     (set! ,fv0 (+ ,fv0 ,fv1))
                     (halt ,fv0))
                  (and (unique? (list fv0 fv1 fv2))
                       (andmap fvar? (list fv0 fv1 fv2)))))))

(define (a2-flatten-begins-test-suite passes flatten-begins)
  (test-suite
   "a2 flatten-begins tests"

   (let ([x `(begin (halt 5))])
     (check-match (flatten-begins x)
                  `(begin (halt 5)))
     (check-from flatten-begins passes x 5))

   (let ([x `(begin (begin (halt 5)))])
     (check-match (flatten-begins x)
                  `(begin (halt 5)))
     (check-from flatten-begins passes x 5))

   (let ([x `(begin (begin (set! fv0 1)
                           (set! fv1 2))
                    (set! fv0 (+ fv0 fv1))
                    (halt fv0))])
     (check-match (flatten-begins x)
                  `(begin (set! fv0 1)
                          (set! fv1 2)
                          (set! fv0 (+ fv0 fv1))
                          (halt fv0)))
     (check-from flatten-begins passes x 3))

   (let ([x `(begin (begin (set! fv0 1)
                           (set! fv1 2))
                    (begin (begin (set! fv0 (+ fv0 fv1))))
                    (halt fv0))])
     (check-match (flatten-begins x)
                  `(begin (set! fv0 1)
                          (set! fv1 2)
                          (set! fv0 (+ fv0 fv1))
                          (halt fv0)))
     (check-from flatten-begins passes x 3))))

(define (a2-patch-instructions-test-suite passes patch-instructions)
  (test-suite
   "a2 patch-instructions tests"

   (let ([x `(begin (halt 5))])
     (check-match (patch-instructions x)
                  `(begin (set! rax 5)))
     (check-from patch-instructions passes x 5))

   (let ([x `(begin (halt fv0))])
     (check-match (patch-instructions x)
                  `(begin (set! rax fv0))))

   (let ([x `(begin (set! fv0 fv1)
                    (halt 5))])
     (check-match (patch-instructions x)
                  `(begin (set! ,p1 fv1)
                          (set! fv0 ,p1)
                          (set! rax 5))
                  ;; must coerce into boolean!
                  (and (andmap (curryr memv (current-patch-instructions-registers))
                               (list p1))
                       #t)))

   (let ([x `(begin (set! r12 (+ r12 r8))
                    (halt 1))])
     (test-case "Fragile test; allowed to fail"
       (check-match (patch-instructions x)
                    `(begin
                       (set! r12 (+ r12 r8))
                       (set! rax 1))))
     (check-from patch-instructions passes x 1))

   (let ([x `(begin (set! fv0 (+ fv0 1))
                    (halt 1))])
     (test-case "Fragile test; allowed to fail"
       (check-match (patch-instructions x)
                    `(begin (set! ,p1 fv0)
                            (set! ,p1 (+ ,p1 1))
                            (set! fv0 ,p1)
                            (set! rax 1))
                    (and (andmap (curryr memv (current-patch-instructions-registers))
                                 (list p1))
                         #t)))
     (check-from patch-instructions passes x 1))

   (let ([x `(begin (set! fv0 (+ fv0 fv1))
                    (halt 1))])
     (test-case "Fragile test; allowed to fail"
       (check-match (patch-instructions x)
                    `(begin (set! ,p1 fv0)
                            (set! ,p2 fv1)
                            (set! ,p1 (+ ,p1 ,p2))
                            (set! fv0 ,p1)
                            (set! rax 1))
                    (and (andmap (curryr memv (current-patch-instructions-registers))
                                 (list p1 p2))
                         #t)))
     (check-from patch-instructions passes x 1))

   (let ([x `(begin
               (set! fv1 7)
               (set! fv2 0)
               (set! r8 5)
               (set! r12 fv1)
               (set! r12 (+ r12 r8))
               (set! fv2 (+ fv2 4))
               (halt fv2))])
     (test-case "Fragile test; allowed to fail"
       (check-match (patch-instructions x)
                    `(begin
                       (set! fv1 7)
                       (set! fv2 0)
                       (set! r8 5)
                       (set! r12 fv1)
                       (set! r12 (+ r12 r8))
                       (set! ,p1 fv2)
                       (set! ,p1 (+ ,p1 4))
                       (set! fv2 ,p1)
                       (set! rax fv2))
                    (and (andmap (curryr memv (current-patch-instructions-registers))
                                 (list p1))
                         #t)))
     (check-from patch-instructions passes x 4))

   (let ([x `(begin
               (set! rbx 0)
               (set! rcx 0)
               (set! r9 42)
               (set! rbx (+ rbx rcx))
               (set! rbx (+ rbx r9))
               (halt rbx))])
     (test-case "Fragile test; allowed to fail"
       (check-match (patch-instructions x)
                    `(begin
                       (set! rbx 0)
                       (set! rcx 0)
                       (set! r9 42)
                       (set! rbx (+ rbx rcx))
                       (set! rbx (+ rbx r9))
                       (set! rax rbx))))
     (check-from patch-instructions passes x 42))

   (let ([x `(begin
               (set! fv0 2)
               (set! fv1 5)
               (set! fv0 (+ fv0 fv1))
               (halt fv0))])
     (check-match (patch-instructions x)
                  `(begin
                     (set! fv0 2)
                     (set! fv1 5)
                     (set! ,p1 fv0)
                     (set! ,p2 fv1)
                     (set! ,p1 (+ ,p1 ,p2))
                     (set! fv0 ,p1)
                     (set! rax fv0))
                  (and (andmap (curryr memv (current-patch-instructions-registers))
                               (list p1 p2))
                       #t))
     (check-from patch-instructions passes x 7))))

(define (a2-implement-fvars-test-suite passes implement-fvars)
  (test-suite
   "a2 implement-fvars tests"

   (let ([x `(begin
               (set! rax 5))])
     (check-match (implement-fvars x)
                  `(begin
                     (set! rax 5)))
     (check-from implement-fvars passes x 5))

   (let ([x `(begin
               (set! rax fv0))])
     (check-match (implement-fvars x)
                  `(begin
                     (set! rax (rbp - 0)))))

   (let ([x `(begin
               (set! rax fv3))])
     (check-match (implement-fvars x)
                  `(begin
                     (set! rax (rbp - 24)))))

   (let ([x `(begin
               (set! fv1 1)
               (set! rax 1))])
     (check-match (implement-fvars x)
                  `(begin
                     (set! (rbp - 8) 1)
                     (set! rax 1)))
     (check-from implement-fvars passes x 1))

   (let ([x `(begin
               (set! rax 1)
               (set! fv1 rax))])
     (check-match (implement-fvars x)
                  `(begin
                     (set! rax 1)
                     (set! (rbp - 8) rax)))
     (check-from implement-fvars passes x 1))

   (let ([x `(begin
               (set! fv1 1)
               (set! fv0 2)
               (set! rax 1)
               (set! rax (+ rax fv1))
               (set! rax (* rax fv0)))])
     (check-match (implement-fvars x)
                  `(begin
                     (set! (rbp - 8) 1)
                     (set! (rbp - 0) 2)
                     (set! rax 1)
                     (set! rax (+ rax (rbp - 8)))
                     (set! rax (* rax (rbp - 0)))))
     (check-from implement-fvars passes x 4)))

   (let ([x `(begin
               (set! r11 0)
               (set! fv0 2)
               (set! fv1 0)
               (set! rsi r11)
               (set! rsi (+ rsi fv0))
               (set! rsi (+ rsi 0))
               (set! rdi rsi)
               (set! rdi (+ rdi fv1))
               (set! rax rdi))])
     (check-match (implement-fvars x)
                  `(begin
                     (set! r11 0)
                     (set! (rbp - 0) 2)
                     (set! (rbp - 8) 0)
                     (set! rsi r11)
                     (set! rsi (+ rsi (rbp - 0)))
                     (set! rsi (+ rsi 0))
                     (set! rdi rsi)
                     (set! rdi (+ rdi (rbp - 8)))
                     (set! rax rdi)))
     (check-from implement-fvars passes x 2))

   (let ([x `(begin
               (set! fv5 2)
               (set! rax fv5)
               (set! rax (+ rax fv5))
               (set! fv5 rax)
               (set! rax fv5))])
     (check-match (implement-fvars x)
                  `(begin
                     (set! (rbp - 40) 2)
                     (set! rax (rbp - 40))
                     (set! rax (+ rax (rbp - 40)))
                     (set! (rbp - 40) rax)
                     (set! rax (rbp - 40))))

     (check-from implement-fvars passes x 4)))

(define (a2-paren-x64-v2-test-suite passes interp-paren-x64)
  (define pass-list (current-pass-list))

  (test-suite
   "a2 paren-x64 tests"
   #:before
   (thunk
    (current-pass-list passes))
   #:after
   (thunk
    (current-pass-list pass-list))

   (let ([x `(begin
               (set! rax 5))])
     (check-confluent?/upto (execute x) (interp-paren-x64 x) 5))

   (let ([x `(begin
               (set! (rbp - 0) 1)
               (set! rax (rbp - 0)))])
     (check-confluent?/upto (execute x) (interp-paren-x64 x) 1))

   (let ([x `(begin
               (set! (rbp - 24) 24)
               (set! rax (rbp - 24)))])
     (check-confluent?/upto (execute x) (interp-paren-x64 x) 24))

   (let ([x `(begin
               (set! rax 1)
               (set! (rbp - 8) rax))])
     (check-confluent?/upto (execute x) (interp-paren-x64 x) 1))

   (let ([x `(begin
               (set! (rbp - 8) 1)
               (set! (rbp - 0) 2)
               (set! rax 1)
               (set! rax (+ rax (rbp - 8)))
               (set! rax (* rax (rbp - 0))))])
     (check-confluent?/upto (execute x) (interp-paren-x64 x) 4))

   (let ([x `(begin
               (set! r11 0)
               (set! (rbp - 0) 2)
               (set! (rbp - 8) 0)
               (set! rsi r11)
               (set! rsi (+ rsi (rbp - 0)))
               (set! rsi (+ rsi 0))
               (set! rdi rsi)
               (set! rdi (+ rdi (rbp - 8)))
               (set! rax rdi))])
     (check-confluent?/upto (execute x) (interp-paren-x64 x) 2))

   (let ([x `(begin
               (set! r9 0)
               (set! r12 0)
               (set! r9 (* r9 r12))
               (set! r9 (* r9 0))
               (set! r9 (+ r9 1))
               (set! rcx r9)
               (set! rcx (* rcx 1))
               (set! rax 7)
               (set! rax (+ rax rcx))
               (set! rax (+ rax rax))
               (set! r14 2)
               (set! r14 (+ r14 rax))
               (set! rax (* rax r14)))])
     (check-confluent?/upto (execute x) (interp-paren-x64 x) 288))

   (let ([x `(begin
               (set! r11 0)
               (set! r8 2)
               (set! rcx 0)
               (set! rsi r11)
               (set! rsi (+ rsi r8))
               (set! rsi (+ rsi 0))
               (set! rdi rsi)
               (set! rdi (+ rdi rcx))
               (set! rax rdi))])
     (check-confluent?/upto (execute x) (interp-paren-x64 x) 2))))

(define (a2-values-lang-test-suite passes interp-values-lang)
  (define pass-list (current-pass-list))

  (test-suite
   "a2 values-lang tests"
   #:before
   (thunk
    (current-pass-list passes))
   #:after
   (thunk
    (current-pass-list pass-list))

   (let ([x `(module 5)])
     (check-confluent?/upto (execute x) (interp-values-lang x) 5))

   (let ([x `(module (+ 1 2))])
     (check-confluent?/upto (execute x) (interp-values-lang x) 3))

   (let ([x `(module (* 3 2))])
     (check-confluent?/upto (execute x) (interp-values-lang x) 6))

   (let ([x `(module (let ([x 10]) x))])
     (check-confluent?/upto (execute x) (interp-values-lang x) 10))

   (let ([x `(module (let ([foo 1]) (+ foo foo)))])
     (check-confluent?/upto (execute x) (interp-values-lang x) 2))

   (let ([x `(module (let ([x 10])
                       (let ([y 12])
                         (+ x y))))])
     (check-confluent?/upto (execute x) (interp-values-lang x) 22))

   (let ([x `(module (let ([foo 1]
                           [bar 2])
                       (+ foo bar)))])
     (check-confluent?/upto (execute x) (interp-values-lang x) 3))

   (let ([x `(module (let ([foo 1])
                       (let ([bar foo])
                         bar)))])
     (check-confluent?/upto (execute x) (interp-values-lang x) 1))

   (let ([x `(module (let ([foo (let ([bar 1]) bar)]
                           [bar 2])
                       (+ foo bar)))])
     (check-confluent?/upto (execute x) (interp-values-lang x) 3))

   (let ([x `(module (let ([foo 1])
                       (let ([foo 2])
                         foo)))])
     (check-confluent?/upto (execute x) (interp-values-lang x) 2))

   (let ([x `(module (let ([foo 1])
                       (let ([foo (+ 1 foo)]
                             [bar (+ 2 foo)])
                         (+ foo bar))))])
     (check-confluent?/upto (execute x) (interp-values-lang x) 5))

   (let ([x `(module
                 (let ([x 3]
                       [y 5]
                       [z (let ([q (* 4 2)]
                                [r (+ 5 6)])
                            (+ q r))])
                   (let ([m (+ x y)])
                     (* m z))))])
     (check-confluent?/upto (execute x) (interp-values-lang x) 152))

   (let ([x `(module
                 (let ([x 1]
                       [y (let ([z 3]) z)]
                       [z (let ([y 2]) (+ y y))])
                   (+ y z)))])
     (check-confluent?/upto (execute x) (interp-values-lang x) 7))))

(define (a2-public-test-suite pass-ls
                              uncover-locals
                              assign-fvars
                              replace-locations
                              check-paren-x64
                              interp-values-lang
                              interp-paren-x64)
  (match-define
    (list
     check-values-lang
     uniquify
     sequentialize-let
     canonicalize-bind
     select-instructions
     assign-homes
     flatten-begins
     patch-instructions
     implement-fvars
     generate-x64
     wrap-x64-run-time
     wrap-x64-boilerplate)
    pass-ls)

  (define passes
    (list
     check-values-lang
     uniquify
     sequentialize-let
     canonicalize-bind
     select-instructions
     uncover-locals
     assign-fvars
     replace-locations
     flatten-begins
     patch-instructions
     implement-fvars
     generate-x64
     wrap-x64-run-time
     wrap-x64-boilerplate))

  (define decoder (current-actual-decoder))
  (define masker (current-expected-masker))
  (define encoder (current-input-encoder))
  (define run/read (current-run/read))

  (test-suite
   "a2 public test suite"
   #:before
   (thunk
    (current-run/read nasm-run/print-number))
   #:after
   (thunk
    (current-run/read run/read))

   (let ([passes (list
                  check-paren-x64
                  generate-x64
                  wrap-x64-run-time
                  wrap-x64-boilerplate)])
     (test-suite
      "a2, backward compat a1 tests"
      (a1-check-paren-x64-syntax-test-suite check-paren-x64)
      (a1-paren-x64-test-suite passes interp-paren-x64)))

   (a2-paren-x64-v2-test-suite (list
                                check-paren-x64
                                generate-x64
                                wrap-x64-run-time
                                wrap-x64-boilerplate)
                               interp-paren-x64)

   (a2-implement-fvars-test-suite passes implement-fvars)
   (a2-patch-instructions-test-suite passes patch-instructions)
   (a2-flatten-begins-test-suite passes flatten-begins)
   (a2-replace-locations-test-suite passes replace-locations)
   (a2-assign-fvars-test-suite passes assign-fvars)
   (a2-uncover-locals-test-suite passes uncover-locals)
   (a2-select-instructions-test-suite passes select-instructions)
   (a2-assign-homes-test-suite passes assign-homes)
   (a2-canonicalize-bind-test-suite passes canonicalize-bind)
   (a2-sequentialize-let-test-suite passes sequentialize-let)
   (a2-uniquify-test-suite passes uniquify)
   (a2-check-values-lang-test-suite pass-ls check-values-lang)
   (a2-values-lang-test-suite pass-ls interp-values-lang)))
