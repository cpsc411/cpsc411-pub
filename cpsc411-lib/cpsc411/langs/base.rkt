#lang racket/base

(require
 cpsc411/machine-ints
 racket/local
 (for-syntax
  racket/base
  racket/dict
  racket/syntax
  syntax/parse)
 (only-in racket/base [define r:define]))

(provide
 (rename-out [new-define define])
 (except-out (all-defined-out) new-define))

(compile-allow-set!-undefined #t)

(begin-for-syntax
  (define (infostx->dict stx)
    (map (compose (lambda (p)
                    `(,(syntax->datum (car p))
                      . ,(car (cdr p))))
                  syntax->list) (syntax->list stx)))

  (define (bind-info info e)
    (let ([info (infostx->dict info)])
      #`(let #,(for/list ([loc (apply
                                append
                                (syntax->list (dict-ref info 'locals #'()))
                                (map syntax->list (syntax->list (dict-ref info 'new-frames #'()))))])
                 #`[#,loc (void)])
          #,e))))

(define-syntax (bind-regs stx)
  (syntax-parse stx
    [(_ tail)
     #:with rax (syntax-local-introduce (format-id #f "~a" 'rax))
     #:with (regs ...)
     (map
      (lambda (x) (syntax-local-introduce (format-id #f "~a" x)))
      '(rsp rbp rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))
     #:with (vals ...)
     #'((void) (void) (void) (void) (void) (void) (void) (void) (void)
               (void) (void) (void) (void) (void)
               (lambda () rax))
     #`(let ([rax (void)])
         (let ([regs vals] ...)
           tail))]))

(define-syntax (bind-fvars stx)
  (syntax-parse stx
    [(_ n:nat tail)
     #:with (fvars ...)
     (for/list ([i (in-range 0 (syntax->datum #'n))])
       (syntax-local-introduce (format-id #f "fv~a" i)))
     #`(let ([fvars (void)] ...)
         tail)]))

;; TODO: Use of ~datum is bad should be ~literal
(define-syntax (module stx)
  (syntax-parse stx
    [(module (~and (~var defs) ((~datum define) _ ...)) ...)
     #:with ((define label tail) rdefs ...) (attribute defs)
     #`(module () rdefs ... tail)]
    [(module (~and (~var defs) ((~datum define) _ ...)) ... tail)
     #`(module () defs ... tail)]
    [(module info defs ... tail)
     #`(begin
         (compile-allow-set!-undefined #t)
         (bind-fvars 1000 (bind-regs #,(bind-info #'info #`(local [defs ...] tail)))))]))

(define-syntax-rule (jump l rest ...)
  (l))

(define-syntax (new-define stx)
  (syntax-parse stx
    [(_ name (~and (~var body) ((~literal lambda) _ ...)))
     #`(define name body)]
    [(_ name body)
     #`(define name (lambda () body))]
    [(_ name info body)
     #`(define name (lambda () #,(bind-info #'info #'body)))]))

(define-syntax (return-point stx)
  (syntax-parse stx
    [(_ label tail)
     #:with rax (format-id stx "~a" 'rax)
     #`(begin
         (let/cc label tail)
         rax)]))

(define (call f . ops)
  (apply f ops))

(define + x64-add)
(define - x64-sub)
(define * x64-mul)
(define halt values)

(define (true) #t)
(define (false) #f)
(define (!= e1 e2) (not (= e1 e2)))
(define (nop) (void))


(module+ interp
  (provide interp-base)
  (require (only-in racket (define r:define)))
  (define-syntax-rule (define e ...) (new-define e ...))

  (define-namespace-anchor a)
  (r:define interp-base
    (let ([ns (namespace-anchor->namespace a)])
      (lambda (x)
        (eval x ns)))))

#;(define-syntax (rbp stx)
    (syntax-parse stx
      [_:id
       ]))
