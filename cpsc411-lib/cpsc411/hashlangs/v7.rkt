#lang racket/base

;; TODO: copy/pasted from base because I'm bad at this hash-lang pattern.
(require
 (submod "../langs/v7.rkt" safe-langs)
 (for-syntax racket/base))

(provide
 require
 only-in
 local-require
 (all-from-out (submod "../langs/v7.rkt" safe-langs))
 (rename-out [new-top-interaction #%top-interaction])
 #%datum
 #%app
 (rename-out [new-module-begin #%module-begin]))
