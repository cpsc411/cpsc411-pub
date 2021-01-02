#lang racket/base

(require
 syntax/readerr
 "../test-suite/utils.rkt")

(provide
 (rename-out
  [$-read read]
  [$-read-syntax read-syntax]))

(define ($-read in)
  (parameterize ([current-readtable (make-$-readtable)])
    (read in)))

(define ($-read-syntax src in)
  (parameterize ([current-readtable (make-$-readtable)])
    (read-syntax src in)))

(define (make-$-readtable)
  (make-readtable (current-readtable) #\$ 'terminating-macro read-dollar))

(define read-dollar
  (case-lambda
   [(ch in)
    ((current-input-encoder) (read in))]
   [(ch in src line col pos)
    (let ([val (read-syntax src in)])
      (datum->syntax #f (syntax->datum #`,((current-input-encoder) #,val))))]))
