#lang racket/base

(require
 (submod "../langs/v7.rkt" safe-langs)
 (for-syntax racket/base))

(provide
 require
 only-in
 local-require
 #;(all-from-out "../langs/base.rkt")
 (all-from-out (submod "../langs/v7.rkt" safe-langs))
 (rename-out [new-top-interaction #%top-interaction])
 #%datum
 #%app
 (rename-out [new-module-begin #%module-begin]))
