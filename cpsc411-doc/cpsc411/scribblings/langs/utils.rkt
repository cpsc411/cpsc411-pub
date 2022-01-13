#lang at-exp racket/base
(require
 (for-syntax racket/base racket/function racket/syntax)
 (for-label racket/base racket/contract)
 scribble/bettergrammar
 scribble/manual)

(provide deflangs)

(define-syntax (deflangs stx)
  (syntax-case stx ()
    [(_ langs ...)
     (with-syntax ([(preds? ...) (map (curry format-id stx "~a?") (syntax->list #'(langs ...)))]
                   [(interps ...) (map (curry format-id stx "interp-~a") (syntax->list #'(langs ...)))])
       #`(begin
           #,@(for/list ([pred? (syntax->list #'(preds? ...))]
                         [lang (syntax->list #'(langs ...))]
                         [interp (syntax->list #'(interps ...))])
                #`(begin
                    (defthing #:kind "" #,lang grammar? (bettergrammar* #,lang))
                    (defproc (#,pred? [a any/c]) boolean? @elem{Decides whether @racket[a] is a valid program in the } @racket[#,lang] " grammar, represented as a " @racket[quote] "d datum. The first non-terminal in the grammar defines valid programs.")
                    (defproc (#,interp [a #,pred?]) any/c @elem{Evaluates a } @racket[#,lang] " program to a value.")))))]))
