#lang racket/base

(require
 "a6.rkt")

(provide (all-defined-out))

(module safe-langs racket/base
  (require
   "base.rkt"
   cpsc411/machine-ints
   (only-in racket/base
            [module+ r:module+] [define r:define]))

  (provide
   (all-from-out "base.rkt")
   (all-defined-out))

  (r:define (wrap-error-ret who unsafe)
    (lambda ops
      (with-handlers ([exn:fail:contract? (λ (e) (error who))])
        (apply unsafe ops))))

  (define-syntax-rule (define-error-ret f unsafe)
    (r:define f (wrap-error-ret 'f unsafe)))

  (define-error-ret + unsafe-fx+)
  (define-error-ret - unsafe-fx-)
  (define-error-ret * unsafe-fx*)
  (define-error-ret < unsafe-fx<)
  (define-error-ret > unsafe-fx>)
  (define-error-ret <= unsafe-fx<=)
  (define-error-ret >= unsafe-fx>=)

  (r:module+ interp
             (provide interp-exprs-lang-v7)
             (define-namespace-anchor a)
             (r:define interp-exprs-lang-v7
                       (let ([ns (namespace-anchor->namespace a)])
                         (lambda (x)
                           (eval x ns))))))

(require (submod 'safe-langs interp))
(provide interp-exprs-lang-v7)
