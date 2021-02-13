#lang racket/base

(require
 "a3.rkt")

(provide
 (all-defined-out))

(require (submod "a2.rkt" m interp))
(require (submod "base.rkt" interp))

(define interp-values-lang-v4 interp-values-lang-v3)
(define interp-values-lang-unique-v4 interp-values-lang-v4)
(define interp-imp-mf-lang-v4 interp-values-lang-unique-v4)
(define interp-imp-cmf-lang-v4 interp-imp-mf-lang-v4)
(define interp-asm-pred-lang-v4 interp-base)
(define interp-asm-pred-lang-v4/locals interp-asm-pred-lang-v4)
(define interp-asm-pred-lang-v4/undead interp-asm-pred-lang-v4/locals)
(define interp-asm-pred-lang-v4/conflicts interp-asm-pred-lang-v4/undead)
(define interp-asm-pred-lang-v4/assignments interp-asm-pred-lang-v4/conflicts)
(define interp-nested-asm-lang-v4 interp-values-lang-v3)

(module block-langs racket/base
  (require
   "base.rkt"
   (only-in racket/base [module+ r:module+] [define r:define]))
  (provide
   (all-from-out "base.rkt")
   (all-defined-out))

  (define-syntax-rule (define l tail)
    (r:define (l) tail))

  (define-syntax-rule (module (define l tail) b ...)
    (begin
      (define l tail)
      b ...
      (l)))

  (define-syntax-rule (jump l)
    (l))

  (r:module+ interp
    (provide interp-block-pred-lang-v4)
    (define-namespace-anchor a)
    (r:define interp-block-pred-lang-v4
      (let ([ns (namespace-anchor->namespace a)])
        (lambda (x)
          (eval x ns))))))

(require (submod 'block-langs interp))
(provide interp-block-pred-lang-v4)

(define interp-block-asm-lang-v4 interp-block-pred-lang-v4)

(define interp-para-asm-lang-v4 interp-block-asm-lang-v4)
