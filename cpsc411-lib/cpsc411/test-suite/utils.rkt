#lang racket/base

(require
 (for-syntax racket/base)
 racket/engine
 racket/function
 racket/match
 racket/list
 racket/set
 rackunit
 cpsc411/compiler-lib
 (submod "../langs/base.rkt" interp)
 "../langs/v1.rkt"
 "../langs/v2.rkt"
 "../langs/v3.rkt"
 "../langs/v4.rkt"
 "../langs/v5.rkt"
 "../langs/v6.rkt"
 "../langs/v7.rkt"
 "../langs/v8.rkt"
 "../langs/v9.rkt")

(provide
 (all-defined-out))

(define ((ptr? tag mask) v)
  (eq? tag (bitwise-and v mask)))

(define fixnum-ptr? (ptr? (current-fixnum-tag) (current-fixnum-mask)))
(define true-ptr? (curry eq? (current-true-ptr)))
(define false-ptr? (curry eq? (current-false-ptr)))
(define empty-ptr? (curry eq? (current-empty-ptr)))
(define error-ptr? (ptr? (current-error-tag) (current-error-mask)))
(define ascii-ptr? (ptr? (current-ascii-char-tag) (current-ascii-char-mask)))

(define cons-ptr? (ptr? (current-pair-tag) (current-pair-mask)))
(define vector-ptr? (ptr? (current-vector-tag) (current-vector-mask)))

;; TODO Finish
(define (ptr->v v)
  (match v
    [(? true-ptr?)
     #t]
    [(? false-ptr?)
     #f]
    [(? fixnum-ptr?)
     (arithmetic-shift v -3)]
    [(? empty-ptr?)
     '()]
    [(? error-ptr?)
     (arithmetic-shift v -8)]
    [(? ascii-ptr?)
     (integer->char (arithmetic-shift v -8))]
    ;; hm.
    [(? vector-ptr?)
     (make-vector 0)]
    [(? cons-ptr?)
     (cons 0 0)]))

;; dictionary of function pointers to mutable sets of (list string? Program)
;; function pointer is an interpreter from cpsc411/langs, identifies the language the programs longs to.
;; the mutable set is a set of lists of names, a test programs for that
;; language, and the expected value (optional)
(define test-prog-dict
  (make-hasheq))

(require racket/contract)
(define (register-test-programs! interp progs)
  #;(-> (-> any/c any/c)
      (listof
       (or/c (list/c string? list?)
             (list/c string? list?
                     (or/c int64?
                           char?
                           pair?
                           vector?
                           void?))))
      void?)
  (when (eq? interp interp-base)
    (error "Trying to register programs to base interpreter"))
  (hash-update!
   test-prog-dict
   interp
   (lambda (u)
     (begin
       (set-union! u (list->mutable-set
                      (for/list ([entry progs])
                        (if (= 3 (length entry))
                            entry
                            (match entry
                              [(list name prog)
                               (list name prog (interp prog))])))))
       u))
   (mutable-set)))

;; map from interpreters to validates for the source language
(define validator-dict
  (hasheq
   interp-paren-x64-v1 paren-x64-v1?

   interp-asm-lang-v2 asm-lang-v2?
   interp-asm-lang-v2/locals asm-lang-v2/locals?
   interp-asm-lang-v2/assignments asm-lang-v2/assignments?
   interp-nested-asm-lang-v2 nested-asm-lang-v2?
   interp-para-asm-lang-v2 para-asm-lang-v2?
   interp-paren-x64-fvars-v2 paren-x64-fvars-v2?
   interp-paren-x64-v2 paren-x64-v2?

   interp-values-lang-v3 values-lang-v3?
   interp-values-unique-lang-v3 values-unique-lang-v3?
   interp-imp-mf-lang-v3 imp-mf-lang-v3?
   interp-imp-cmf-lang-v3 imp-cmf-lang-v3?

   interp-values-lang-v4 values-lang-v4?
   interp-values-unique-lang-v4 values-unique-lang-v4?
   interp-imp-mf-lang-v4 imp-mf-lang-v4?
   interp-imp-cmf-lang-v4 imp-cmf-lang-v4?
   interp-asm-pred-lang-v4 asm-pred-lang-v4?
   interp-asm-pred-lang-v4/locals asm-pred-lang-v4/locals?
   interp-asm-pred-lang-v4/undead asm-pred-lang-v4/undead?
   interp-asm-pred-lang-v4/conflicts asm-pred-lang-v4/conflicts?
   interp-asm-pred-lang-v4/assignments asm-pred-lang-v4/assignments?
   interp-nested-asm-lang-v4 nested-asm-lang-v4?
   interp-block-pred-lang-v4 block-pred-lang-v4?
   interp-block-asm-lang-v4 block-asm-lang-v4?
   interp-para-asm-lang-v4 para-asm-lang-v4?
   interp-paren-x64-fvars-v4 paren-x64-fvars-v4?
   interp-paren-x64-v4 paren-x64-v4?

   interp-values-lang-v5 values-lang-v5?
   interp-values-unique-lang-v5 values-unique-lang-v5?
   interp-imp-mf-lang-v5 imp-mf-lang-v5?
   interp-proc-imp-cmf-lang-v5 proc-imp-cmf-lang-v5?
   interp-imp-cmf-lang-v5 imp-cmf-lang-v5?
   interp-asm-pred-lang-v5 asm-pred-lang-v5?
   interp-asm-pred-lang-v5/locals asm-pred-lang-v5/locals?
   interp-asm-pred-lang-v5/undead asm-pred-lang-v5/undead?
   interp-asm-pred-lang-v5/conflicts asm-pred-lang-v5/conflicts?
   interp-asm-pred-lang-v5/assignments asm-pred-lang-v5/assignments?
   interp-nested-asm-lang-v5 nested-asm-lang-v5?
   interp-block-pred-lang-v5 block-pred-lang-v5?
   interp-block-asm-lang-v5 block-asm-lang-v5?
   interp-para-asm-lang-v5 para-asm-lang-v5?
   interp-paren-x64-fvars-v5 paren-x64-fvars-v5?
   interp-paren-x64-v5 paren-x64-v5?

   interp-values-lang-v6 values-lang-v6?
   interp-values-unique-lang-v6 values-unique-lang-v6?
   interp-imp-mf-lang-v6 imp-mf-lang-v6?
   interp-proc-imp-cmf-lang-v6 proc-imp-cmf-lang-v6?
   interp-imp-cmf-lang-v6 imp-cmf-lang-v6?
   interp-asm-pred-lang-v6 asm-pred-lang-v6?
   interp-asm-pred-lang-v6/locals asm-pred-lang-v6/locals?
   interp-asm-pred-lang-v6/undead asm-pred-lang-v6/undead?
   interp-asm-pred-lang-v6/conflicts asm-pred-lang-v6/conflicts?
   interp-asm-pred-lang-v6/pre-framed asm-pred-lang-v6/pre-framed?
   interp-asm-pred-lang-v6/framed asm-pred-lang-v6/framed?
   interp-asm-pred-lang-v6/spilled asm-pred-lang-v6/spilled?
   interp-asm-pred-lang-v6/assignments asm-pred-lang-v6/assignments?
   interp-nested-asm-lang-fvars-v6 nested-asm-lang-fvars-v6?
   interp-nested-asm-lang-v6 nested-asm-lang-v6?
   interp-block-pred-lang-v6 block-pred-lang-v6?
   interp-block-asm-lang-v6 block-asm-lang-v6?
   interp-para-asm-lang-v6 para-asm-lang-v6?
   interp-paren-x64-v6 paren-x64-v6?


   interp-exprs-lang-v7 exprs-lang-v7?
   interp-exprs-unique-lang-v7 exprs-unique-lang-v7?
   interp-exprs-unsafe-data-lang-v7 (list exprs-unsafe-data-lang-v7?
                                          (lambda (sv tv)
                                            (equal? sv (ptr->v tv))))
   interp-exprs-bits-lang-v7 exprs-bits-lang-v7?
   interp-values-bits-lang-v7 values-bits-lang-v7?
   interp-imp-mf-lang-v7 imp-mf-lang-v7?
   interp-proc-imp-cmf-lang-v7 proc-imp-cmf-lang-v7?
   interp-imp-cmf-lang-v7 imp-cmf-lang-v7?
   interp-asm-pred-lang-v7 asm-pred-lang-v7?
   interp-asm-pred-lang-v7/locals asm-pred-lang-v7/locals?
   interp-asm-pred-lang-v7/undead asm-pred-lang-v7/undead?
   interp-asm-pred-lang-v7/conflicts asm-pred-lang-v7/conflicts?
   interp-asm-pred-lang-v7/pre-framed asm-pred-lang-v7/pre-framed?
   interp-asm-pred-lang-v7/framed asm-pred-lang-v7/framed?
   interp-asm-pred-lang-v7/spilled asm-pred-lang-v7/spilled?
   interp-asm-pred-lang-v7/assignments asm-pred-lang-v7/assignments?
   interp-nested-asm-lang-fvars-v7 nested-asm-lang-fvars-v7?
   interp-nested-asm-lang-v7 nested-asm-lang-v7?
   interp-block-pred-lang-v7 block-pred-lang-v7?
   interp-block-asm-lang-v7 block-asm-lang-v7?
   interp-para-asm-lang-v7 para-asm-lang-v7?
   interp-paren-x64-v7 (list paren-x64-v7?
                             (lambda (sv tv)
                               (equal? (ptr->v sv) tv)))

   interp-exprs-lang-v8 exprs-lang-v8?
   interp-exprs-unique-lang-v8 exprs-unique-lang-v8?
   interp-exprs-unsafe-data-lang-v8 (list exprs-unsafe-data-lang-v8?
                                          (lambda (sv tv)
                                            (cond
                                              [(vector? sv)
                                               (vector? (ptr->v tv))]
                                              [(cons? sv)
                                               (cons? (ptr->v tv))]
                                              [else
                                               (equal? sv (ptr->v tv))])))
   interp-exprs-bits-lang-v8 exprs-bits-lang-v8?
   interp-values-bits-lang-v8 values-bits-lang-v8?
   interp-imp-mf-lang-v8 imp-mf-lang-v8?
   interp-proc-imp-cmf-lang-v8 proc-imp-cmf-lang-v8?
   interp-imp-cmf-lang-v8 imp-cmf-lang-v8?
   interp-asm-alloc-lang-v8
   (list asm-alloc-lang-v8?
         (lambda (sv tv)
           (cond
             [(vector? (ptr->v sv))
              (vector? (ptr->v tv))]
             [(cons? (ptr->v sv))
              (cons? (ptr->v tv))]
             [else
              (equal? sv tv)])))
   interp-asm-pred-lang-v8 asm-pred-lang-v8?
   interp-asm-pred-lang-v8/locals asm-pred-lang-v8/locals?
   interp-asm-pred-lang-v8/undead asm-pred-lang-v8/undead?
   interp-asm-pred-lang-v8/conflicts asm-pred-lang-v8/conflicts?
   interp-asm-pred-lang-v8/pre-framed asm-pred-lang-v8/pre-framed?
   interp-asm-pred-lang-v8/framed asm-pred-lang-v8/framed?
   interp-asm-pred-lang-v8/spilled asm-pred-lang-v8/spilled?
   interp-asm-pred-lang-v8/assignments asm-pred-lang-v8/assignments?
   interp-nested-asm-lang-fvars-v8 nested-asm-lang-fvars-v8?
   interp-nested-asm-lang-v8 nested-asm-lang-v8?
   interp-block-pred-lang-v8 block-pred-lang-v8?
   interp-block-asm-lang-v8 block-asm-lang-v8?
   interp-para-asm-lang-v8 para-asm-lang-v8?
   interp-paren-x64-mops-v8 paren-x64-mops-v8?
   interp-paren-x64-v8 (list paren-x64-v8?
                             (lambda (sv tv)
                               (cond
                                 [(vector? (ptr->v sv))
                                  (vector? tv)]
                                 [(cons? (ptr->v sv))
                                  (cons? tv)]
                                 [else
                                  (equal? (ptr->v sv) tv)])))

   interp-exprs-lang-v9 exprs-lang-v9?
   interp-exprs-unique-lang-v9 exprs-unique-lang-v9?
   interp-exprs-unsafe-data-lang-v9 exprs-unsafe-data-lang-v9?
   interp-exprs-unsafe-lang-v9 exprs-unsafe-lang-v9?
   interp-just-exprs-lang-v9 just-exprs-lang-v9?
   interp-lam-opticon-lang-v9 lam-opticon-lang-v9?
   interp-lam-free-lang-v9 lam-free-lang-v9?
   interp-closure-lang-v9 closure-lang-v9?
   interp-hoisted-lang-v9 hoisted-lang-v9?
   interp-proc-exposed-lang-v9 (list proc-exposed-lang-v9?
                                     (lambda (sv tv)
                                       (cond
                                         [(vector? sv)
                                          (vector? (ptr->v tv))]
                                         [(cons? sv)
                                          (cons? (ptr->v tv))]
                                         [else
                                          (equal? sv (ptr->v tv))])))))

(define (static-compose f1 f2)
  (cond
    [(eq? f1 values)
     f2]
    [(eq? f2 values)
     f1]
    [else (compose f1 f2)]))

;; milliseconds
(define current-test-case-timeout (make-parameter 5000))

;; Milliseconds (() -> any_1) (() -> any_2) -> any_1 or any_2
;; Runs proc in an engine, returning its result, or calling the failure
;; continuation of proc fails to finish before timeout-ms milliseconds.
(define (run-test-with-timeout proc
                          [timeout-ms (current-test-case-timeout)]
                          [fail-k (lambda () (fail-check "Timed out"))])
  (let* ([e (engine proc)]
         [res (engine-run timeout-ms e)])
    (if res
        (engine-result e)
        (fail-k))))

(define-syntax-rule (with-timeout stx ...)
  (run-test-with-timeout (lambda (_) stx ...)))

(define-check (check-timeout interp program)
  (with-check-info (['interp (object-name interp)])
    (let ([res (gensym)])
      (unless (eq? res (run-test-with-timeout
                        (lambda (_) (interp program))
                        (current-test-case-timeout)
                        (lambda () res)))
        (fail-check)))))

;; public test suite:
;; non-adversarial; can assume interpreter list is valid

;; Prepass:
;; 1. Start from source of each chapter
;; 2. Take list of passes (abstract), list of matching (Maybe interpreter): equal length
;; 3. Append final interpreter, which is (lambda (x) (parameterize ([current-pass-list (list values)]) (execute x)))
;;    now, always 1 more interpreter than pass
;; 4. Compose all passes whose target language has a #f interpreter

;; Maintain mutable dictionary of interpret -> progs
;;
;; Testing process
;; For each pass, source-interp, target-interp
;; - look up list of programs by source-interp
;; - for each test-prog
;;   - compile, compare output with interpreters
;;   - validate output by validator
;;   - if passes, add output to list of programs for target-interp

(define (find-passes-by-interp interp-src pass-ls interp-ls)
  (for/list ([pass pass-ls]
             [interp interp-ls]
             #:when (eq? interp interp-src))
    pass))

(define (test-compiler-pass pass src-interp trg-interp trg-validator [src-equiv equal?])
  (when (eq? trg-interp interp-base)
    (error "interp-base used as target interpreter"))
  (define target-interp-progs (hash-ref! test-prog-dict trg-interp (mutable-set)))
  (for ([test-prog-entry (hash-ref! test-prog-dict src-interp (mutable-set))]
        [i (in-naturals)])
    (define name (first test-prog-entry))
    (define test-prog (second test-prog-entry))
    (define expected (third test-prog-entry))
    (with-check-info (['test-program test-prog]
                      ['expected expected]
                      ['src-interp (object-name src-interp)]
                      ['trg-interp (object-name trg-interp)])
      (test-case (format "~a suite" (or (object-name pass) 'anonymous))
        (define _output (box (void)))
        (with-check-info (['test-type "Checking test-program compiles without error"])
          (check-not-exn
           (thunk
            (with-timeout
              (set-box! _output (pass test-prog))))))
        (define output (unbox _output))
        (with-check-info (['output-program output])
          (with-check-info (['type-type "Checking that output is syntactically correct"])
            (when trg-validator
              (check-true (trg-validator output))))
          (define actual (box (void)))
          (with-check-info (['test-type "Checking output is interpretable"])
            (check-not-exn
             (thunk (with-timeout
                      (set-box! actual (trg-interp output))))))
          (with-check-info (['test-type "Checking that output produces correct value"])
            (check src-equiv expected (unbox actual)))
          (set-add! target-interp-progs `(,name ,output ,(unbox actual))))))))

(define (compiler-testomatic _pass-ls _interp-ls [run/read (current-run/read)])
  (unless (eq? (length _pass-ls) (length _interp-ls))
    (error "Compiler Testomatic expects the pass list to be the same length as the interpreter list."
           _pass-ls
           _interp-ls))

  (define-values (pass-ls interp-ls _)
    (for/foldr ([pass-ls '()]
                [interp-ls (list (lambda (x)
                                   (parameterize ([current-pass-list (list values)])
                                     (execute x run/read))))]
                [pass-so-far values])
        ([pass _pass-ls]
         [interp _interp-ls])
      (if interp
          (values (cons (static-compose pass-so-far pass) pass-ls)
                  (cons interp interp-ls)
                  values)
          (values pass-ls
                  interp-ls
                  (static-compose pass-so-far pass)))))

  (test-suite
   ""
   (let loop ([pass-ls pass-ls]
              [interp-ls interp-ls])
     (cond
       [(empty? pass-ls)
        (void)]
       [else
        (define pass (first pass-ls))
        (define src-interp (first interp-ls))
        (define trg-interp (second interp-ls))
        (define src-extras (hash-ref validator-dict src-interp #f))
        ;; src programs are assumed to be valid, so ignore src validator
        (define-values (_1 src-equiv)
          (match src-extras
            [`(,src-validator ,src-equiv)
             (apply values src-extras)]
            [(? procedure?)
             (values src-extras equal?)]
            [_ (values #f equal?)]))
        (define trg-extras (hash-ref validator-dict trg-interp #f))
        ;; src directs the equivalence, so ignore trg equiv
        (define-values (trg-validator _2)
          (match trg-extras
            [`(,trg-validator ,trg-equiv)
             (apply values trg-extras)]
            [(? procedure?)
             (values trg-extras equal?)]
            [_ (values #f equal?)]))
        (test-compiler-pass pass src-interp trg-interp trg-validator src-equiv)
        (loop (rest pass-ls) (rest interp-ls))]))))


;; OLD INFRASTRUCTURE
;; ------------------------------------------------------------------------

(define current-input-encoder (make-parameter (lambda (x) x)))
(define current-actual-decoder (make-parameter (lambda (x) x)))
(define current-expected-masker (make-parameter (lambda (x) x)))

; Disables fragile/feedback-only tests, typically for grading but also if you
; just want to.
(define current-enable-grading (make-parameter #f))

(define-syntax-rule (fragile-test-case e)
  (if (current-enable-grading)
      (void)
      (test-case "Fragile test; failure allowed"
        e)))

(define-check (check-validator f e)
  (with-check-info (['validator f]
                    ['source e])
    (with-handlers ([values (lambda (e)
                             (fail (exn-message e)))])
      (check-equal? (f e) e))))

(define-check (test-validator name f e)
  (test-suite
   name
   (test-begin
     (check-validator f e))))

(define-check (test-validator-exn name f e)
  (test-suite name (test-begin (check-validator-exn f e))))

(define-check (check-validator-exn f e)
  (with-check-info (['validator f]
                    ['term e])
    (check-exn exn:fail? (thunk (f e)))))

(define-check (check-equal?/mask f1 f2 e1 e2)
  (with-check-info (['raw-actual e1]
                    ['raw-expected e2]
                    ['decode-actual f1]
                    ['mask-expected f2]
                    ['actual-decoded (f1 e1)]
                    ['expected-masked (f2 e2)])
    (check-equal? (f1 e1) (f2 e2))))

(define-check (check-equal?/upto e1 e2)
  (check-equal?/mask (current-actual-decoder) (current-expected-masker) e1 e2))

(define-check (check-confluent?/mask decode mask compiled interpreted expected)
  (with-check-info (['raw-compiled compiled]
                    ['raw-interpreted interpreted]
                    ['raw-expected expected]
                    ['decode-actual decode]
                    ['mask-expected mask]
                    ['compiled-decoded (decode compiled)]
                    ['interpreted-decoded (decode interpreted)]
                    ['expected-masked (mask expected)])
    (match (list (equal? (decode compiled) (mask expected))
                 (equal? (decode interpreted) (mask expected))
                 (equal? (decode compiled) (decode interpreted)))
      ; compiled correct?, interpreted correct?, consistent?
      ['(#f #f #f) (fail-check "neither compiled nor interpreted are equal to expected, and they aren't equal to each other")]
      ['(#f #f #t) (fail-check "neither compiled nor interpreted are equal to expected, but they are equal to each other")]
      ['(#f #t #f) (fail-check "compiled isn't equal to expected, but interpreted is")]
      ['(#f #t #t) (fail-check "impossible - compiled isn't equal to expected, but is equal to interpreted, which is equal to expected")]
      ['(#t #f #f) (fail-check "interpreted isn't equal to expected, but compiled is")]
      ['(#t #f #t) (fail-check "impossible - interpreted isn't equal to expected, but is equal to compiled, which is equal to expected")]
      ['(#t #t #f) (fail-check "impossible - compiled and interpreted are equal to expected, but not to each other")]
      ['(#t #t #t) (void)])))

(define-check (check-confluent?/upto compiled interpreted expected)
  (check-confluent?/mask (current-actual-decoder) (current-expected-masker) compiled interpreted expected))

(define-syntax (test-confluent?/mask stx)
  (syntax-case stx ()
    [(_ decode mask compiled interpreted expected)
     (quasisyntax/loc stx
       (test-suite
        ""
        (test-begin
          (with-handlers ([values (lambda (e) #,(quasisyntax/loc stx
                                                 (fail (exn-message e))))])
            (with-check-info (['raw-compiled compiled]
                              ['raw-expected expected]
                              ['decode-actual decode]
                              ['mask-expected mask]
                              ['compiled-decoded (decode compiled)]
                              ['expected-masked (mask expected)])
              (check-equal? (decode compiled) (mask expected)))))

        (test-begin
          (with-handlers ([values (lambda (e) #,(quasisyntax/loc stx
                                                  (fail (exn-message e))))])
            (with-check-info (['raw-interpreted interpreted]
                              ['raw-expected expected]
                              ['decode-actual decode]
                              ['mask-expected mask]
                              ['interpreted-decoded (decode interpreted)]
                              ['expected-masked (mask expected)])
              (check-equal? (decode interpreted) (mask expected)))))

        (test-begin
          (with-handlers ([values (lambda (e) #,(quasisyntax/loc stx
                                                  (fail (exn-message e))))])
            (with-check-info (['raw-compiled compiled]
                              ['raw-interpreted interpreted]
                              ['decode-actual decode]
                              ['mask-expected mask]
                              ['compiled-decoded (decode compiled)]
                              ['interpreted-decoded (decode interpreted)])
              (check-equal? (decode compiled) (decode interpreted)))))))]))

(define-syntax-rule (test-confluent?/upto compiled interpreted expected)
  (test-confluent?/mask (current-actual-decoder) (current-expected-masker) compiled interpreted expected))

(define-check (check-correct interp1 interp2 source target)
  (with-check-info (['source-interpreter interp1]
                    ['target-interpreter interp2]
                    ['source source]
                    ['target target])
    (with-handlers ([values (lambda (e) (fail (exn-message e)))])
      (check-equal? (interp2 target) (interp1 source)))))

(define-syntax (test-correct stx)
  (syntax-case stx ()
    [(_  interp1 interp2 s t)
     (quasisyntax/loc stx
       (test-begin
         (with-check-info (['source-interpreter interp1]
                           ['target-interpreter interp2]
                           ['source s]
                           ['target t]
                           ['name 'test-correct])
           (with-handlers ([values (lambda (e)
                                     #,(quasisyntax/loc stx
                                         (fail (exn-message e))))])
             #,(quasisyntax/loc stx
                 (check-equal? (interp2 t) (interp1 s)))))))]))

(define-check (check-from pass pass-ls actual expected)
  (parameterize ([current-pass-list (member pass pass-ls)])
    (with-check-info (['pass-ls (member pass pass-ls)]
                      ['source actual]
                      ['expected expected])
      (with-handlers ([values (lambda (e) (fail (exn-message e)))])
        (check-equal?/upto (execute actual) expected)))))

(define-syntax (test-from stx)
  (syntax-case stx ()
    [(_ pass pass-ls actual expected)
     (quasisyntax/loc stx
       (test-begin
         #,(quasisyntax/loc stx
             (check-from pass pass-ls actual expected))))]))

(define-check (check-against-ref student-passes ref-passes program)
  (let ([expected (parameterize ([current-pass-list ref-passes])
                    (execute program))])
    (for-each (λ (student-pass index)
                (let ([prefix (take ref-passes index)]
                      [suffix (drop ref-passes (add1 index))])
                  (parameterize ([current-pass-list (append prefix (list student-pass) suffix)])
                    (check-equal? (execute program) expected))))
              student-passes
              (range (length student-passes)))))

(define-syntax-rule (test-against-ref student-passes ref-passes program)
   (test-begin
     (let ([expected (parameterize ([current-pass-list ref-passes])
                     (execute program))])
     (for-each (λ (student-pass index)
                 (let ([prefix (take ref-passes index)]
                       [suffix (drop ref-passes (add1 index))])
                   (parameterize ([current-pass-list (append prefix (list student-pass) suffix)])
                     (check-equal? (execute program) expected))))
               student-passes
               (range (length student-passes))))))
  #;(syntax-case stx ()
    [(_ student-passes ref-passes program)
     (quasisyntax/loc stx
       )])

(define exit-code-mask (lambda (x) (modulo x 256)))

(define-check (check-import-list mod ls)
  (for ([i ls])
    (check-not-exn
     (thunk (dynamic-require mod i fail)))))

(define (unique? lst)
  (not (check-duplicates lst)))

(define-syntax (test-match stx)
  (syntax-case stx ()
    [(_ s ...)
     (quasisyntax/loc stx
         #,(quasisyntax/loc stx
             (test-begin (check-match s ...))))]))

;; Make a bunch of intermediate tests
(define (make-tests name-tag
                    ir-progs-ls
                    expected-ls
                    passes . test-passes)
  (make-test-suite
   ""
   (for/list ([pass test-passes]
              [progs ir-progs-ls]
              #:when (unless (member pass passes)
                       (printf "Warning: Couldn't test from ~a, as it wasn't found in the current-pass-list~n" pass)
                       #f))
     (test-suite
      (format "~a from ~a tests" name-tag (object-name pass))
      (for ([t progs]
            [expected expected-ls])
        (test-from pass passes t expected))))))
