#lang racket/base

(require "a2.rkt")

(provide
 (all-from-out "a2.rkt")
 (all-defined-out))

(define interp-asm-lang-v2/undead interp-asm-lang-v2)
(define interp-asm-lang-v2/conflicts interp-asm-lang-v2)
