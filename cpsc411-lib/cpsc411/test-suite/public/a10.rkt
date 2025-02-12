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
 ;"../../langs/a9.rkt"
 #;"a8.rkt")

(provide (all-defined-out))

(define current-test-timeout (make-parameter 2000))
(define current-omega-timeout (make-parameter 5000))
(define current-omega-stack-limit (make-parameter 16))
(define current-big-fact-stack-limit (make-parameter 32))

(struct exn:fail:timeout exn:fail ())

(define non-zero? (and/c integer? (not/c zero?)))

(define-inline (valid-p expected p . rest)
  `((,expected . (,p ,@rest))
    (,zero? . (,p ,nasm-run/exit-code))))

(define-inline (valid-expr expected expr . rest)
  `((,expected . ((module ,expr) ,@rest))
    (,zero? . ((module ,expr) ,nasm-run/exit-code))))

(define nasm-run/print-string/filter
  (lambda (x)
    (parameterize ([current-error-port
                    (open-output-nowhere)])
      (nasm-run/print-string x))))

(define-inline (invalid-expr code expr)
  `((,code . ((module ,expr) ,nasm-run/exit-code))
    ("" . ((module ,expr) ,nasm-run/print-string/filter))))

(define-inline (make-valid-expr-tests ls)
  (reverse
   (for/fold ([tests '()])
             ([x ls])
     (append (apply valid-expr x) tests))))

(define-inline (make-invalid-expr-tests ls)
  (reverse
   (for/fold ([tests '()])
             ([x ls])
     (append (apply invalid-expr x) tests))))

(define tests
  ;; (listof (name . (listof (flat-contract x execute args))))
  `(

     ("Closure Tests"
      ,@(make-valid-expr-tests
         `((5 (let ([x 5]) ((lambda (y) x) 2)))
           (,(+ 5 6 7)
            (let ([x 5] [y 6] [z 7])
              ((lambda (a) (+ (+ x y) z)) 2)))))
      ,@(valid-p
         (curry equal? `((1 1 1 1 1)
                         .
                         (6 5 4 3 2)))
         `(module
            (define* (zero? n)
              (eq? n 0))

            (define* (sub1 n)
              (- n 1))

            (define* (curry f x)
              (lambda (y)
                (f x y)))

            (define* (build-list f n)
              (if (zero? n)
                  '()
                  (cons (f n) (build-list f (sub1 n)))))

            (cons
             (build-list (lambda (n) 1) 5)
             (build-list (curry + 1) 5)))))

     ))

(define (a10-public-test-suite)

  (define decoder (current-actual-decoder))
  (define masker (current-expected-masker))
  (define encoder (current-input-encoder))
  (define run/read (current-run/read))

  (make-test-suite
   "a11 public test suite"

   (append
    (for/list ([suite tests])
      (make-test-suite
       (car suite)
       (for/list ([case (cdr suite)]
                  [i (in-naturals 1)])
         (test-suite
          (format "Test ~a" i)
          (with-check-info (['expected (car case)]
                            ['actual-expr (cdr case)])
            (test-begin
              (let ([e (engine (lambda (_) (apply execute (cdr case))))])
                (if (engine-run (current-test-timeout) e)
                    (with-check-info (['actual (engine-result e)])
                      (check-pred (flat-contract-predicate (car case)) (engine-result e)))
                    (with-check-info (['engine-result (engine-result e)])
                      (fail "Test timed out"))
                    ;; TODO: Test suites broken
                    #;(raise (exn:fail:timeout "Test timed out"
                                               (current-continuation-marks)))))))))))

    (list
     (test-suite
      "a11 tail calls"
      (test-case "Omega"
        (let ([stack-limit (current-omega-stack-limit)]
              [str (compile '(module ((lambda (x) (x x))
                                      (lambda (x) (x x)))))])
          (let-values ([(out in pid err control-f)
                        (apply
                         values
                         ((nasm-run/observe
                           #:block? #f
                           (lambda (x)
                             (process (format "ulimit -Ss ~a -Hs ~a; ~a" stack-limit stack-limit x))))
                          str))])
            (let ([e (engine (lambda (_) (control-f 'wait) (control-f 'exit-code)))])
              (if (engine-run (current-omega-timeout) e)
                  (if (eq? 0 (engine-result e))
                      (fail "Omega terminated normally")
                      (with-check-info (['ulimit-exit (engine-result e)])
                        (fail "Omega ate too much memory")))
                  (control-f 'kill))))))

      (test-case "Big Fact"
        (let ([stack-limit (current-big-fact-stack-limit)]
              [str (compile '(module
                                 (define* (fact n acc)
                                   (if (eq? n 0)
                                       acc
                                       (fact (- n 1) (* n acc))))
                                 (fact 19 1)))])
          (check-equal?
           ((nasm-run/observe (lambda (x)
                                (with-input-from-string
                                  (with-output-to-string
                                    (thunk
                                     (let ([exit-code
                                            (system/exit-code (format "ulimit -Ss ~a -Hs ~a; ~a"
                                                                      stack-limit
                                                                      stack-limit
                                                                      x))])
                                       (if (zero? exit-code)
                                           (void)
                                           (with-check-info (['ulimit-exit exit-code])
                                             (fail "Fact ate too much memory"))))))
                                  read))) str)

           121645100408832000))))))

   #:before
   (thunk
    (current-run/read nasm-run/read))

   #:after
   (thunk
    (current-run/read run/read))))
