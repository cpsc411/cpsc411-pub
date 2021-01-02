#lang racket/base

;; Defines rackunit extensions and wrappers for testing CPSC 411.
;; Many of these add "keep on chugging" semantics to rackunit, to enable unit
;; testing to continue after failure.
;; This helps provide better feedback to students.
;; S

(require
 (prefix-in rackunit ru:))

(provide
 (all-defined-out)
 (all-from-out rackunit))

; check-equal? where output should match original input
(define (ch-eq f e m)
  (test-begin (check-equal? (f e) e m)))

; check-exn
(define (ch-exn f e m)
  (test-begin (check-exn exn:fail? (thunk (f e)) m)))

; check-equal? with distinct output
(define (ch-eq-output f e o m)
  (test-begin (check-equal? (f e) o m)))

; check-equal? with distinct output where
; input and output are normalized for whitespace
(define (ch-eq-normal f e o m)
  (let ([s string-normalize-spaces])
    (test-begin (check-equal? (s (f e)) (s o) m))))

; wrapper for checks without message parameter (ex. check-match)
(define (wrap-msg m c)
  (with-check-info (['message (string-info m)]) c))

; special check for interp-values-lang
; - ensures output matches execute
; - verifies actual output is as intended
(define (ch-interp input r m)
  (and (test-begin (check-equal? (interp-values-lang input) (execute input) m))
       (test-begin (check-equal? (interp-values-lang input) r m))))
