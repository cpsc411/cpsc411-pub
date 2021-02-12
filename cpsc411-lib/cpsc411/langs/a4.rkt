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
(define interp-nested-asm-lang-v4 interp-asm-pred-lang-v4/assignments)
