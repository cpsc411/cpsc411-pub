#lang racket/base

(require
 cpsc411/machine-ints
 syntax/parse)

(provide
 module
 begin
 +
 -
 *
 halt)

(compile-allow-set!-undefined #t)

(define-syntax-rule (module _ tail)
  (begin tail))

(define + x64-add)
(define - x64-sub)
(define * x64-mul)
(define halt values)

(module+ interp
  (provide interp-base)
  (define-namespace-anchor a)
  (define interp-base
    (let ([ns (namespace-anchor->namespace a)])
      (lambda (x)
        (eval x ns)))))

#;(define-syntax (rbp stx)
    (syntax-parse stx
      [_:id
       ]))
