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
 (only-in racket/base [define r:define] [error r:error] [set! r:set!])
 (only-in racket/list empty))

(provide
 ;; NOTE: must mirror renaming in base interpreter in module+ below.
 (rename-out [new-define define])
 (rename-out [new-lambda lambda])
 (rename-out [new-begin begin])
 (except-out (all-defined-out)
             new-define
             new-+
             new--
             new-*)
 #;(rename-out [new-module-begin #%module-begin])
 #;#%top-interaction
 #;#%datum
 #;#%app
 (rename-out
  [new-+ +]
  [new-- -]
  [new-* *])
 =
 >
 <
 <=
 >=

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

#;(compile-allow-set!-undefined #t)

(define stack (make-vector 1000 'unalloc))
(define fbp (box (sub1 (vector-length stack))))

(define-syntax (rbp stx)
  (syntax-parse stx
    [:id
     #'(unbox fbp)]
    [(base (~datum -) offset)
     #`(vector-ref stack (- (unbox fbp) offset))]))

(define current-fvar-offset (box 0))
(define (inc-fvar-offset! x)
  (set-box! current-fvar-offset (+ x (unbox current-fvar-offset))))

(define (init-stack)
  (begin
    (set-box! fbp (sub1 (vector-length stack)))
    (set! stack (make-vector 1000 'unalloc))))

(begin-for-syntax
  ;; TODO: Using let-syntax, this become a real performance issue.
  ;; Should probably do a quick traversal of the whole program to find the
  ;; largest fvar.
  ;; 50 seems.. okay. about 25% slower than 5.
  ;; 1000 is unusable.
  (define current-fvars (make-parameter 25))

  (define (infostx->dict stx)
    (map (compose (lambda (p)
                    `(,(syntax->datum (car p))
                      . ,(car (cdr p))))
                  syntax->list) (syntax->list stx)))

  (require racket/trace)
  (define (get-info-bound-vars info)
    (apply append
           ;; see bind-info; if new-frames exist, don't bind assignments
           (if (dict-ref (infostx->dict info) 'new-frames #f)
               '()
               (map (compose car syntax->list) (syntax->list (dict-ref (infostx->dict info) 'assignment #'()))))
           (map syntax->list (syntax->list (dict-ref (infostx->dict info) 'new-frames #'())))))

  (define (bind-info info e)
    (let ([info (infostx->dict info)])
      (define tail
        (for/fold ([e e])
                  ([new-frame (map syntax->list (syntax->list (dict-ref info 'new-frames #'())))])
          #`(let-syntax #,(for/list ([loc new-frame]
                                     [i (in-naturals 0)])
                            (with-syntax ([fvar (format-id e "fv~a" i)])
                              #`[#,loc (make-set!-transformer
                                        (lambda (stx)
                                          (syntax-case stx ()
                                            [(set! id v)
                                             #`(r:set! fvar v)]
                                            [id (identifier? #'id) #'fvar])))]))
              #,e)))
      ;; if there's a new-frame, don't use assignments, since the new frame
      ;; hasn't be allocated yet and using fvars for call-undead will be
      ;; inconsistent with using them for new-frames
      (if (dict-ref info 'new-frames #f)
          tail
          #`(let-syntax #,(for/list ([assignments (map syntax->list (syntax->list
                                                                     (dict-ref
                                                                      info
                                                                      'assignment
                                                                      #'())))])
                            (with-syntax ([aloc (car assignments)]
                                          [rloc (cadr assignments)])
                              #`[aloc
                                 (make-set!-transformer
                                  (lambda (stx)
                                    (syntax-case stx ()
                                      [(set! id v)
                                       #`(r:set! rloc v)]
                                      [id (identifier? #'id)
                                          #'rloc])))]))
              #,tail))))

  (define (bind-regs k tail)
    (with-syntax* ([rax (syntax-local-introduce (format-id #f "~a" 'rax))]
                   [(regs ...)
                    (map
                     (lambda (x) (syntax-local-introduce (format-id #f "~a" x)))
                     '(rsp rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))]
                   [(vals ...)
                    #`((void) (void) (void) (void) (void) (void) (void) (void)
                              (void) (void) (alloc 5000) (void) (void)
                              (lambda () (#,k rax)))])
      #`(let ([rax (void)])
          (let ([regs vals] ...)
            #,tail))))

  (define (bind-fvars n tail)
    #`(let-syntax #,(for/list ([i (in-range 0 n)])
                      (with-syntax ([fvar (syntax-local-introduce (format-id #f "fv~a" i))]
                                    [offset (* i 8)])
                        #`[fvar (make-set!-transformer
                                 (lambda (stx)
                                   (syntax-case stx ()
                                     [(set! id v)
                                      #`(vector-set! stack (- (unbox fbp)
                                                              (+ (unbox current-fvar-offset)
                                                                 offset))
                                                     v)]
                                     [id (identifier? #'id)
                                         #`(rbp -
                                                (+ (unbox current-fvar-offset)
                                                   offset))])))]))
        #,tail)
    #;(with-syntax ([(fvars ...)
                   (for/list ([i (in-range 0 n)])
                     (syntax-local-introduce (format-id #f "fv~a" i)))])
      #`(let ([fvars (void)] ...)
          #,tail))))

(define-syntax-rule (new-module-begin stx ...)
  (#%module-begin
   (module stx ...)))

#;(require (for-syntax racket/pretty))
;; TODO: Use of ~datum is bad should be ~literal
(define-syntax (module stx)
  (syntax-parse stx
    #:literals (module)
    ;; NB: Work around an issue that can happen when interpreters are called
    ;; incorrectly, e.g., from stubs.
    ;; Really, should put contracts on the individual interpreters.
    [(module (module r ...))
     #`(module r ...)]
    [(module (~and (~var defs) ((~datum define) _ ...)) ...)
     #:with ((define label tail) rdefs ...) (attribute defs)
     #`(module () defs ... (label))]
    [(module (~and (~var defs) ((~datum define) _ ...)) ... tail)
     #`(module () defs ... tail)]
    [(module info defs ... tail)
     #;(pretty-display
      (syntax->datum
       (local-expand
        (bind-fvars
         (current-fvars)
         (bind-regs
          #'k
          (bind-info
           #'info
           #`(local [defs ...
                      (define #,(syntax-local-introduce (format-id #f "done"))
                        (lambda () (k rax)))
                      (define #,(syntax-local-introduce (format-id #f "halt"))
                        (lambda (x)
                          (begin
                            (set! rax x)
                            (k rax))))]
               (do-bind-locals tail
                               #,@(get-info-bound-vars #'info))))))
        'expression
        '())))
     #`(let/ec k
         (begin
           (init-heap)
           (init-stack)
           #;(compile-allow-set!-undefined #t)
           #,(bind-fvars
              (current-fvars)
              (bind-regs
               #'k
               (bind-info
                #'info
                #`(local [defs ...
                           (define #,(syntax-local-introduce (format-id #f "done"))
                             (lambda () (k rax)))
                           (define #,(syntax-local-introduce (format-id #f "halt"))
                             (lambda (x)
                               (begin
                                 (set! rax x)
                                 (k rax))))]
                    (do-bind-locals tail
                                    #,@(get-info-bound-vars #'info))))))))]))

(define (!= e1 e2) (not (= e1 e2)))

(define
 flags
 (make-hash
  (list
   (cons != #f)
   (cons = #f)
   (cons < #f)
   (cons <= #f)
   (cons > #f)
   (cons >= #f))))

(define (compare v1 v2)
  (for-each (lambda (cmp)
              (hash-set! flags cmp (cmp v1 v2)))
            (list != = < <= > >=)))

;; NOTE: Assumes halt is always called when jumps are used.
;; Some way to forces these to be escape continuations?
(define (jump-if flag d)
  (when (hash-ref flags flag)
    (d)
    (r:error "Shouldn't have returned!")))

;; allow jump to ignore its "arguments"
(define-syntax-rule (jump f rest ...)
  (begin
    (set-box! current-fvar-offset 0)
    (#%app f)))

(begin-for-syntax
  (define (labelify-begin defs ss)
    (syntax-parse ss
      #:datum-literals (with-label begin)
      [((begin ss1 ...) ss ...)
       (labelify-begin defs #`(ss1 ... ss ...))]
      [((with-label l s) ss ...)
       (let-values ([(defs e) (labelify-begin defs #`(s ss ...))])
         (values
          (cons #`(l (lambda () (begin #,@e))) defs)
          #`((l))))]
      [(s ss ...)
       (if (null? (attribute ss))
           (values
            defs
            #`(s))
           (let-values ([(defs e) (labelify-begin defs (attribute ss))])
             (values
              defs
              #`(s #,@e))))])))

;; NOTE: Assumes no nested begins, I think.
(define-syntax (new-begin stx)
  (let-values ([(defs e) (labelify-begin '() (cdr (syntax->list stx)))])
    (if (null? defs)
        #`(begin #,@e)
        #`(letrec (#,@defs)
            (begin #,@e)))))

(define-syntax (new-define stx)
  (syntax-parse stx
    ;; TODO: For some reason, ~literal new-lambda doesn't work
    ;; Fix because ~datum is fragile.
    [(_ name info ((~datum lambda) (args ...) body))
     #`(define name
         (lambda (args ...)
           (do-bind-locals #,(bind-info #'info #'body) args ...
                           #,@(get-info-bound-vars #'info))))]
    [(_ name ((~datum lambda) (args ...) body))
     #`(define name
         (lambda (args ...) (do-bind-locals body args ...)))]
    [(_ name body)
     #`(define name (lambda () (do-bind-locals body)))]
    [(_ name info body)
     #`(define name
         (lambda ()
           (do-bind-locals #,(bind-info #'info #'body)
                           #,@(get-info-bound-vars #'info))))]))

(begin-for-syntax
  (require syntax/id-set racket/set)
  (define gathered-locals (mutable-free-id-set))

  (define (reset-locals!)
    (set-clear! gathered-locals))

  (define (collect-local! id)
    (set-add! gathered-locals id))

  (define (get-locals)
    (set->list gathered-locals)))

(define-syntax (do-bind-locals stx)
  (syntax-parse stx
    [(_ body except ...)
     (reset-locals!)
     (define b (local-expand #'body 'expression '()))
     #`(let (#,@(for/list ([l (get-locals)]
                           #:unless (set-member? (immutable-free-id-set
                                                  (attribute except))
                                                 l))
                  #`[#,(syntax-local-introduce (format-id #f "~a" l)) (void)]))
         #,b)]))

(define-syntax (return-point stx)
  (syntax-parse stx
    [(_ label tail)
     #`(let/cc l1
         (let ([label
                (let ([saved (unbox current-fvar-offset)])
                  (lambda ()
                    (set-box! current-fvar-offset saved)
                    (l1)))])
           tail))]))

(define (call f . ops)
  (apply f ops))

(define new-+ x64-add)
(define new-- x64-sub)
(define new-* x64-mul)

(require (for-syntax (only-in "../compiler-lib.rkt" aloc?)))

(define-syntax (set! stx)
  (syntax-parse stx
    [(_ (~literal rbp) (binop (~literal rbp) v))
     #`(begin
         ;; NOTE: This binop gets bound to one of the new-* above.
         (inc-fvar-offset! (binop 0 v))
         (set-box! fbp (binop rbp v)))]
    [(_ (~literal rbp) v)
     #`(set-box! fbp v)]
    [(_ v1:id v2)
     (when (aloc? (syntax->datum #'v1))
       (collect-local! #'v1))
     #`(r:set! v1 v2)]
    [(_ ((~literal rbp) - offset) v2)
     #`(vector-set! stack (- (unbox fbp) offset) v2)]
    #;[(_ (base (~datum +) offset) value)
     #`(vector-set! base offset value)]
    #;[(_ (base (~datum -) offset) value)
     #`(vector-set! base (* -1 offset) value)]
    #;[(_ loc (base (~datum +) offset))
     #`(r:set! loc (mref base offset))]
    #;[(_ loc (base (~datum -) offset))
     #`(r:set! loc (mref base (* -1 offset)))]
    #;[(_ loc value)
     #`(r:set! loc value)]))

(define (true) #t)
(define (false) #f)
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

(define (unsafe-mset! base offset value)
  (vector-set! memory (/ (+ base offset) ALIGN) value))

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

(define-syntax new-lambda
  (syntax-rules ()
    [(_ info args tail) (lambda args tail)]
    [(_ args tail) (lambda args tail)]))

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
(define (unsafe-procedure-arity x) (- (procedure-arity x) 1))

(define unsafe-procedure-call call)
(define closure-call call)
(define closure-ref unsafe-procedure-ref)

(define (fill-env proc . es)
  (let ([v (unsafe-procedure-env proc)])
    (for ([e (in-list es)] [i (in-naturals)])
      (vector-set! v i e))))

(define-syntax (cletrec stx)
  (syntax-parse stx
    [(_ ([aloc ((~literal make-closure) label arity es ...)] oths ...) tail)
     #`(let ([aloc (make-procedure label #,(length (syntax->datum #'(es ...))))])
         (cletrec (oths ...)
                  (begin
                    (unless (equal? arity (unsafe-procedure-arity label))
                      (error 'make-closure "arity argument doesn't match label"))
                    (fill-env aloc es ...)
                    tail)))]
    [(_ () tail) #'tail]))

(module+ interp
  (provide interp-base)
  (require (only-in racket (define r:define)))
  ;; TODO: fix this so I don't need to repeat renaming
  (define-syntax-rule (define e ...) (new-define e ...))
  (define-syntax-rule (begin e ...) (new-begin e ...))
  (define-syntax-rule (lambda e ...) (new-lambda e ...))
  (define-syntax-rule (+ e ...) (new-+ e ...))
  (define-syntax-rule (- e ...) (new-- e ...))
  (define-syntax-rule (* e ...) (new-* e ...))

  (define-namespace-anchor a)
  (r:define interp-base
    (let ([ns (namespace-anchor->namespace a)])
      (lambda (x)
        (eval x ns)))))

(module+ test
  (require rackunit)
  (check-eq? (error 5) (error 5)))
