#lang racket/base

(require
 (for-syntax racket/base)
 racket/function
 rackunit)

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
                    ['mask-actual f1]
                    ['mask-expected f2]
                    ['actual-masked (f1 e1)]
                    ['expected-masked (f2 e2)])
    (check-equal? (f1 e1) (f2 e2))))

(define-check (check-equal?/upto e1 e2)
  (check-equal?/mask (current-actual-decoder) (current-expected-masker) e1 e2))

(define exit-code-mask (lambda (x) (modulo x 256)))

(define-check (check-import-list mod ls)
  (for ([i ls])
    (check-not-exn
     (thunk (dynamic-require mod i fail)))))
