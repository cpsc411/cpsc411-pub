#lang racket/base

(provide
 (all-defined-out))

(require (submod "base.rkt" interp))

(define interp-values-lang-v3 interp-base)
(define interp-values-lang-unique-v3 interp-values-lang-v3)
(define interp-mf-lang-v3 interp-values-lang-v3)
(define interp-cmf-lang-v3 interp-values-lang-v3)
(define interp-asm-lang-v2 interp-base)
(define interp-asm-lang-v2/locals interp-asm-lang-v2)
(define interp-asm-lang-v2/assignments interp-asm-lang-v2)
(define interp-nested-asm-lang-v2 interp-asm-lang-v2)
(define interp-para-asm-lang-v2 interp-asm-lang-v2)
(define interp-paren-x64-fvars-v2 interp-asm-lang-v2)
