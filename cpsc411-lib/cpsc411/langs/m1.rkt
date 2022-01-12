#lang racket/base

(provide
 (all-defined-out))

(require (submod "base.rkt" interp))

(define (interp-paren-x64-v1 x)
  (modulo (interp-base `(module () (begin ,x rax))) 256))
