#lang racket/base

(provide
 (all-defined-out)
 interp-values-lang-v3)

(module m racket/base
  (require (except-in "base.rkt" module))
  (provide
   (all-from-out "base.rkt")
   (all-defined-out))

  (define-syntax-rule (module tail)
    (begin tail))

  (module+ interp
    (define-namespace-anchor a)
    (provide interp-values-lang-v3)
    (define interp-values-lang-v3
      (begin
        (let ([ns (namespace-anchor->namespace a)])
          (lambda (x)
            (eval x ns)))))))

(require (submod 'm interp))

(define interp-values-lang-unique-v3 interp-values-lang-v3)
(define interp-mf-lang-v3 interp-values-lang-v3)
(define interp-cmf-lang-v3 interp-values-lang-v3)

(require (submod "base.rkt" interp))

(define interp-asm-lang-v2 interp-base)
(define interp-asm-lang-v2/locals interp-asm-lang-v2)
(define interp-asm-lang-v2/assignments interp-asm-lang-v2)
(define interp-nested-asm-lang-v2 interp-asm-lang-v2)
(define interp-para-asm-lang-v2 interp-asm-lang-v2)
(define interp-paren-x64-fvars-v2 interp-asm-lang-v2)
