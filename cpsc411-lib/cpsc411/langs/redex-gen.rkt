#lang racket/base

(require
  (for-syntax
   racket/base
   racket/list
   racket/function
   syntax/stx
   racket/syntax
   syntax/parse)
  racket/contract
  scribble/manual
  scribble/example
  scribble/bettergrammar
  redex/reduction-semantics)

(provide
 (all-defined-out)
 redex-match?)

#;(require
 (for-syntax racket/trace)
 racket/trace)
#;(begin-for-syntax
  (current-trace-print-args
   (let ([ctpa (current-trace-print-args)])
     (lambda (s l kw l2 n)
       (ctpa s (map syntax->datum l) kw l2 n))))
  (current-trace-print-results
   (let ([ctpr (current-trace-print-results)])
     (lambda (s r n)
       (ctpr s (map syntax->datum r) n)))))
(define-syntax (define-language-from-grammar stx)
  (syntax-case stx ()
    [(_ name grammar literals clauses)
     (syntax-case #'clauses ()
       [((id prod ...)
         ...)
        ;; special handling for any
        (let* ([literals^ (remf (curry free-identifier=? #'any) (syntax->list #'literals))]
               [is-pred? (lambda (x)
                           (ormap values (map (curry free-identifier=? x)
                                              literals^)))])
          (with-syntax ([((prod-side-conditions ...) ...)
                         ; in-pair? is a nesting-depth hack to help workaround
                         (letrec ([loop (lambda (prods [in-pair? #f])
                                          (cond
                                            [(stx-null? prods) prods]
                                            [(identifier? prods)
                                             (if (is-pred? prods)
                                                 #`(side-condition any_1 (#,prods (term any_1)))
                                                 prods)]
                                            [(stx-pair? prods)
                                             (cond
                                               ;; support (#:with-contract pat contract) syntax
                                               [(and (keyword? (syntax-e (stx-car prods)))
                                                     (eq? '#:with-contract
                                                          (syntax-e (stx-car prods))))
                                                #`((side-condition any_1
                                                                   (#,(stx-car
                                                                       (stx-cdr
                                                                        (stx-cdr prods))) (term any_1))))]
                                               ; NOTE: Work-around https://github.com/racket/redex/issues/230
                                               [(and (identifier? (stx-car prods))
                                                     (is-pred? (stx-car prods))
                                                     (stx-pair? (stx-cdr prods))
                                                     (free-identifier=? (stx-car (stx-cdr prods)) #'(... ...)))
                                                (when in-pair?
                                                  (error "This grammar cannot be compiled correctly yet" (syntax->datum #'name) (map syntax->datum prods)))
                                                #`(side-condition (any_1 (... ...)) (andmap #,(stx-car prods) (term (any_1 (... ...)))))]
                                               [else
                                                #`(#,(loop (stx-car prods) in-pair?)
                                                   #,@(loop (stx-cdr prods)
                                                            #t))])]
                                            [else prods]))])
                           (stx-map loop #'((prod ...) ...)))])
            #`(define-language name
                [id prod-side-conditions ...]
                ...)))])]))

(define-syntax (define-grammar/pred stx)
  (syntax-parse stx
    [(_ name
        (~optional (~seq #:literals lls))
        (~optional (~seq #:datum-literals dls))
        [id def ...] ...)
     #:with nameL (format-id stx "~aL" #'name)
     #:with pred (format-id stx "~a?" #'name)
     #:with ((defs* ...) ...)
     (for/list ([defs (attribute def)])
       (let loop ([defs defs])
         (if (empty? defs)
             defs
             (let ([d (car defs)])
               (syntax-parse d
                 [#:with-contract
                  (cons (datum->syntax stx (syntax->datum #`(#:from-contract #,(car (cdr defs)))))
                        (loop (cdr (cdr (cdr defs)))))]
                 [_ (cons d (loop (cdr defs)))])))))
     #`(begin
         (define-grammar name #:literals (~? lls ()) #:datum-literals (~? dls ()) [id defs* ...] ...)
         (define-language-from-grammar nameL name (~? lls ()) ((id def ...) ...))
         (define pred (redex-match? nameL #,(car (attribute id)))))]))

#;(define-language-from-grammar Values-lang-v4L values-lang-v4)
#;(define check-values-lang (redex-match? Values-lang-v4L p))
