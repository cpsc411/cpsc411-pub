#lang racket/base

(require
 (for-syntax racket/base)
 racket/function
 racket/match
 racket/list
 rackunit
 cpsc411/compiler-lib)

(provide
 (all-defined-out))

(define current-input-encoder (make-parameter (lambda (x) x)))
(define current-actual-decoder (make-parameter (lambda (x) x)))
(define current-expected-masker (make-parameter (lambda (x) x)))

(define-check (check-validator f e)
  (check-equal? (f e) e))

(define-check (test-validator name f e)
  (test-case name (check-validator f e)))

(define-check (test-validator-exn name f e)
  (test-case name (check-validator-exn f e)))

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

(define-check (check-correct interp1 interp2 source target)
  (with-check-info (['source-interpreter interp1]
                    ['target-interpreter interp2]
                    ['source source]
                    ['target target])
    (with-handlers ([values (lambda (e) (fail (exn-message e)))])
      (check-equal? (interp2 target) (interp1 source)))))

(define-check (check-from pass pass-ls actual expected)
  (parameterize ([current-pass-list (member pass pass-ls)])
    (with-check-info (['pass-ls (member pass pass-ls)])
      (check-equal?/upto (execute actual) expected))))

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

(define exit-code-mask (lambda (x) (modulo x 256)))

(define-check (check-import-list mod ls)
  (for ([i ls])
    (check-not-exn
     (thunk (dynamic-require mod i fail)))))

(define (unique? lst)
  (not (check-duplicates lst)))
