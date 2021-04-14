#lang racket/base

(require
 memoize
 cpsc411/machine-ints
 racket/local
 racket/list
 (for-syntax
  racket/base
  racket/dict
  racket/syntax
  syntax/parse)
 (only-in racket/base [define r:define] [error r:error])
 (only-in racket/list empty))

(provide
 (rename-out [new-define define])
 (except-out (all-defined-out) new-define)
 bitwise-and
 fixnum?
 boolean?
 void?
 empty
 empty?
 void
 vector?
 pair?

 cons
 car
 cdr
 make-vector
 vector-length
 vector-set!
 vector-ref)

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
               (void) (void) (alloc 5000) (void) (void)
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
         (init-heap)
         (compile-allow-set!-undefined #t)
         (bind-fvars 1000 (bind-regs #,(bind-info #'info #`(local [defs ...] tail)))))]))

(define-syntax-rule (jump l rest ...)
  (l))

(define-syntax (new-define stx)
  (syntax-parse stx
    [(_ name info ((~literal lambda) (args ...) body))
     #`(define name (lambda (args ...) #,(bind-info #'info #'body)))]
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

(define (unsafe-fx+ x y) (twos-complement-add 61 x y))
(define (unsafe-fx- x y) (twos-complement-sub 61 x y))
(define (unsafe-fx* x y) (twos-complement-mul 61 x y))
(define unsafe-fx<= <=)
(define unsafe-fx>= >=)
(define unsafe-fx< <)
(define unsafe-fx> >)
(define/memo (error n)
  `(error ,n))
(define (error? n) (and (list? n) (eq? (car n) 'error) (int64? (second n))))
(define (arithmetic-shift-right x y) (arithmetic-shift x (- 0 y)))
(define (ascii-char? x)
  (and (char? x) (<= 40 (char->integer x) 176)))

(define unsafe-car car)
(define unsafe-cdr cdr)
(define (unsafe-make-vector size) (make-vector size 'uninitialized))
(define unsafe-vector-length vector-length)
(define unsafe-vector-set! vector-set!)
(define (unsafe-vector-ref vec pos)
  (let ([val (vector-ref vec pos)])
    (when (equal? val 'uninitialized)
      (r:error 'unsafe-vector-ref "attempting to read from uninitialized memory"))
    val))

(define ALIGN 8)
(define memory (make-vector 10000 'un-aloced))
(define hbp (* ALIGN 2))

(define (mset! base offset value)
  (let ([loc (/ (+ base offset) ALIGN)])
    (unless (integer? loc)
      (r:error 'mset! "attempting to write to unaligned memory (base: ~a, offset: ~a)" base offset))
    (when (equal? 'un-aloced (vector-ref memory loc))
      (r:error 'mset! "attempting to write to unallocated memory (base: ~a, offset: ~a)" base offset))
    (vector-set! memory loc value)))

(define (mref base offset)
  (let ([loc (/ (+ base offset) ALIGN)])
    (unless (integer? loc)
      (r:error 'mref "attempting to read from unaligned memory (base: ~a, offset: ~a)" base offset))
    (when (equal? 'un-aloced (vector-ref memory loc))
      (r:error 'mref "attempting to read from unallocated memory (base: ~a, offset: ~a)" base offset))
    (when (equal? 'aloced (vector-ref memory loc))
      (r:error 'mref "attempting to read from uninitialized memory (base: ~a, offset: ~a)" base offset))
    (vector-ref memory loc)))

(define (alloc len8)
  (let ([len (/ len8 ALIGN)]
        [oldhbp hbp])
    (unless (integer? len)
      (r:error 'alloc "attemptying to allocate unaligned memory (len: ~a)" len8))
    (for ([i (in-range len)])
      (vector-set! memory (+ (/ oldhbp ALIGN) i) 'alloced))
    ;; leave buffer space to check over/underflows
    (set! hbp (+ (+ hbp len8) (* ALIGN 3)))
    oldhbp))

(define (init-heap)
  (begin
    (set! hbp (* ALIGN 2))
    (set! memory (make-vector 10000 'un-aloced))))

(define-syntax lambda
  (syntax-rules ()
    [(_ info args tail) (λ args tail)]
    [(_ args tail) (λ args tail)]))

(define-values (procedure-label:prop procedure-label:prop? unsafe-procedure-label)
    (make-impersonator-property 'procedure-label))

(define-values (procedure-env:prop procedure-env:prop? unsafe-procedure-env)
    (make-impersonator-property 'procedure-label))

(define (make-procedure label env-size)
  (impersonate-procedure label #f procedure-label:prop label procedure-env:prop (unsafe-make-vector env-size)))
(define (unsafe-procedure-ref p i)
  (unsafe-vector-ref (unsafe-procedure-env p) i))
(define (unsafe-procedure-set! p i v)
  (unsafe-vector-set! (unsafe-procedure-env p) i v))

(define unsafe-procedure-call call)
(define unsafe-procedure-arity procedure-arity)
(define closure-call call)
(define closure-ref unsafe-procedure-ref)

(define (fill-env proc . es)
  (let ([v (unsafe-procedure-env proc)])
    (for ([e (in-list es)] [i (in-naturals)])
      (vector-set! v i e))))

(define-syntax (cletrec stx)
  (syntax-parse stx
    [(_ ([aloc ((~literal make-closure) label es ...)] oths ...) tail)
     #`(let ([aloc (make-procedure label #,(length (syntax->datum #'(es ...))))])
         (cletrec (oths ...)
           (begin
             (fill-env aloc es ...)
             tail)))]
    [(_ () tail) #'tail]))

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

(module+ test
  (require rackunit)
  (check-eq? (error 5) (error 5)))
