#lang racket/base

(require
 "a3.rkt")

(provide
 (all-defined-out))

(require "a2.rkt")
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

(module label-langs racket/base
  (require
   (submod ".." block-langs)
   (for-syntax
    racket/base
    syntax/parse)
   (only-in racket/base [module+ r:module+] [define r:define] [begin r:begin]))
  (provide
   (all-from-out (submod ".." block-langs))
   (all-defined-out))

  (r:define
    flags
    (make-hash
     (list
      (cons != #f)
      (cons = #f)
      (cons < #f)
      (cons <= #f)
      (cons > #f)
      (cons >= #f))))

  (define-syntax-rule (compare v1 v2)
    (for-each (lambda (cmp)
                (hash-set! flags cmp (cmp v1 v2)))
              (list != = < <= > >=)))

  (define-syntax-rule (jump-if flag d)
    (r:begin
     (when (hash-ref flags flag)
       (d))))

  (begin-for-syntax
    (define (labelify-begin defs ss)
      (syntax-parse ss
        #:datum-literals (with-label)
        [((with-label l s) ss ...)
         (let-values ([(defs e) (labelify-begin defs #`(s ss ...))])
           (values
            (cons #`(l (lambda () #,e)) defs)
            #`(r:begin (l))))]
        [(s ss ...)
         (if (null? (attribute ss))
             (values
              defs
              #`(r:begin s))
             (let-values ([(defs e) (labelify-begin defs (attribute ss))])
               (values
                defs
                #`(r:begin s #,e))))])))

  (define-syntax (begin stx)
    (let-values ([(defs e) (labelify-begin '() (cdr (syntax->list stx)))])
      (datum->syntax
       stx
       (syntax->datum
        #`(r:begin
           (let/ec done
             (letrec (#,@defs
                      [halt (lambda (v) (set! rax v) (done))])
               (r:begin
                #,e
                (done))))
           rax)))))

  (module+ interp
    (provide interp-para-asm-lang-v4)
    (define-namespace-anchor a)
    (r:define interp-para-asm-lang-v4
      (let ([ns (namespace-anchor->namespace a)])
        (lambda (x)
          (eval x ns))))))

(require (submod 'label-langs interp))
(provide interp-para-asm-lang-v4)

(define interp-paren-x64-fvars-v4 interp-para-asm-lang-v4)
