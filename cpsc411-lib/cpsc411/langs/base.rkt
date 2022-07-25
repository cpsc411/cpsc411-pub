#lang racket/base

(require
 memoize
 cpsc411/machine-ints
 racket/local
 racket/list
 static-rename
 (for-syntax
  syntax/transformer
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
 new-module-begin
 new-top-interaction
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

;; ------------------------------------------------------------------------
;; Register file
;; ------------------------------------------------------------------------

;; Each register maps to a top-level box, and variable-like-transformers
;; transform mutations to set-box! and reference to unbox.

(begin-for-syntax
  (define (make-register-transformer reg)
    (make-variable-like-transformer
     #`(unbox #,reg)
     (lambda (stx)
       (syntax-case stx (r:set!)
         [(r:set! bla v)
          #`(set-box! #,reg v)])))))

(define-syntax (define-registers! stx)
  (syntax-case stx ()
    [(_ (regs ...) (boxs ...))
     #`(begin
         #,@(for/list ([r (syntax->list #'(regs ...))]
                       [b (syntax->list #'(boxs ...))])
              #`(begin
                  (define #,b (box (void)))
                  (define-syntax #,r
                    (make-register-transformer #'#,b)))))]))

(define-registers!
  (rax rsp rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)
  (_rax _rsp _rbx _rcx _rdx _rsi _rdi _r8 _r9 _r10 _r11 _r12
  _r13 _r14 _r15))

(define (init-reg-file)
  (for ([reg (list _rax _rsp _rbx _rcx _rdx _rsi _rdi _r8 _r9 _r10 _r11 _r12
                         _r13 _r14 _r15)])
    (set-box! reg (void)))
  (set-box! _r15 done)
  (set-box! _r12 (alloc 1000)))

;; ------------------------------------------------------------------------
;; Initial return point
;; ------------------------------------------------------------------------

;; Every module should capture its current continuation, which should end the
;; entire module, and store it in exit-cont.

(define exit-cont (box r:error))

(define (halt x)
  (set-box! _rax x)
  ((unbox exit-cont) x))

(define (done)
  ((unbox exit-cont) (unbox _rax)))

;; ------------------------------------------------------------------------
;; Stack
;; ------------------------------------------------------------------------

(define stack (make-vector 1000 'unalloc))
(define _rbp (box (sub1 (vector-length stack))))

(define-syntax (rbp stx)
  (syntax-parse stx
    [:id
     #'(unbox _rbp)]
    [(base (~datum -) offset)
     #`(vector-ref stack (- (unbox _rbp) offset))]))

(define current-fvar-offset (box 0))

(define (inc-fvar-offset! x)
  (set-box! current-fvar-offset (+ x (unbox current-fvar-offset))))

(define (init-stack)
  (begin
    (set-box! _rbp (sub1 (vector-length stack)))
    (r:set! stack (make-vector 1000 'unalloc))))

(begin-for-syntax
  (define current-fvars (make-parameter 1000))

  (define (make-fvar-transformer offset)
    (make-variable-like-transformer
     #`(rbp - (+ (unbox current-fvar-offset) #,offset))
     (lambda (stx)
       (syntax-case stx (r:set!)
         [(r:set! bla v)
          #`(vector-set!
             stack
             (- (unbox _rbp)
                (+ (unbox current-fvar-offset) #,offset))
             v)])))))

(define-syntax (define-fvars! stx)
  (syntax-case stx ()
    [(_)
     #`(begin
         #,@(for/list ([i (in-range 0 (current-fvars))])
              (with-syntax ([fvar (syntax-local-introduce (format-id #f "fv~a" i))]
                            [offset (* i 8)])
                #`(define-syntax fvar (make-fvar-transformer offset)))))]))

(define-fvars!)

;; ------------------------------------------------------------------------
;; ... everything else ...
;; ------------------------------------------------------------------------

(begin-for-syntax
  (define (infostx->dict stx)
    (define (convert-assignments info)
      (cond
        [(dict-ref info 'assignment #f)
         =>
         (lambda (assignments)
           (dict-set info 'assignment (map syntax->list assignments)))]
        [else info]))

    (define (convert-new-frames info)
      (cond
        [(dict-ref info 'new-frames #f)
         =>
         (lambda (new-frames)
           (dict-set info 'new-frames (map syntax->list new-frames)))]
        [else info]))

    (define info-dict
      (map (compose (lambda (p)
                      `(,(syntax->datum (car p))
                        . ,(syntax->list (car (cdr p)))))
                    syntax->list) (syntax->list stx)))
    ;; pre-process some well-known info
    (convert-new-frames (convert-assignments info-dict)))

  (define (get-info-bound-vars info)
    (apply append
           ;; see bind-info; if new-frames exist, don't bind assignments
           (if (dict-ref info 'new-frames #f)
               '()
               (map car (dict-ref info 'assignment '())))
           (dict-ref info 'new-frames '())))

  (define (make-aloc-transformer rloc)
    (make-variable-like-transformer
     rloc
     (lambda (stx)
       (syntax-case stx (r:set!)
         [(r:set! bla v)
          #`(r:set! #,rloc v)]))))

  (define (bind-info info e)
    (define tail
      (for/fold ([e e])
                ([new-frame (dict-ref info 'new-frames '())])
        #`(let-syntax #,(for/list ([nfvar new-frame]
                                   [i (in-naturals 0)])
                          #`[#,nfvar (make-fvar-transformer #,(* i 8))])
            #,e)))
    ;; if there's a new-frame, don't use assignments, since the new frame
    ;; hasn't be allocated yet and using fvars for call-undead will be
    ;; inconsistent with using them for new-frames
    (if (dict-ref info 'new-frames #f)
        tail
        #`(let-syntax #,(for/list ([assignments (dict-ref info 'assignment '())])
                          (with-syntax ([aloc (car assignments)]
                                        [rloc (cadr assignments)])
                            #`[aloc (make-aloc-transformer #'rloc)]))
            #,tail))))

(define-syntax-rule (new-module-begin stx ...)
  (#%module-begin
   (module stx ...)))

(define-syntax (new-top-interaction stx)
  (syntax-case stx (require)
    ;; Hack
    [(_ . (require foo))
     #`(#%top-interaction . (require foo))]
    [(_ . tail)
     ;; TODO: For Racket interop, we want to be able to shift the exit explicitly.
     #`(#%top-interaction
        .
        (begin
          (call/cc (lambda (k) (set-box! exit-cont k)))
          tail))]))

;; TODO: Use of ~datum is bad should be ~literal
(define-syntax (module stx)
  (syntax-parse stx
    #:literals (module)
    ;; NB: Work around an issue that can happen when interpreters are called
    ;; incorrectly, e.g., from stubs.
    ;; Really, should put contracts on the individual interpreters.
    [(module)
     #'(void)]
    [(module (module r ...))
     #`(module r ...)]
    [(module (~and (~var defs) ((~datum define) _ ...)) ...)
     #:with ((define label tail) rdefs ...) (attribute defs)
     #`(module () defs ... (label))]
    [(module (~and (~var defs) ((~datum define) _ ...)) ... tail)
     #`(module () defs ... tail)]
    [(module info defs ... tail)
     (define info-dict (infostx->dict #'info))
     #`(let/ec k
         (set-box! exit-cont k)
         (begin
           (init-heap)
           (init-stack)
           (init-reg-file)
           #,(bind-info
              info-dict
              #`(local [defs ...]
                  (do-bind-locals tail
                                  #,@(get-info-bound-vars info-dict))))))]))

(define (!= e1 e2) (not (= e1 e2)))

(define
 flags
 (make-hasheq
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

(define (jump-to f)
  (begin
    (set-box! current-fvar-offset 0)
    (#%app f)))

;; allow jump to ignore its "arguments"
(define-syntax-rule (jump f rest ...)
  (jump-to f))

(define (with-label . rest)
  (error "Cannot use with-label in non-begin context"))

(begin-for-syntax
  (define (labelify-begin defs effects)
    (if (null? effects)
        (values defs '())
        (let ([effect (car effects)]
              [effects (cdr effects)])
          (syntax-parse effect
            #:literals (with-label new-begin)
            [(new-begin effects1 ...)
             (labelify-begin defs (append (attribute effects1) effects))]
            [(with-label label effect1)
             (let-values ([(defs effects^)
                           (labelify-begin defs (cons (attribute effect1) effects))])
               (values
                (cons #`(label (lambda () (begin #,@effects^))) defs)
                (list #`(#%app label))))]
            [_
             (let-values ([(defs effects^) (labelify-begin defs effects)])
               (values
                defs
                (cons effect effects^)))])))))

;; NOTE: Assumes no nested begins, I think.
(define-syntax (new-begin stx)
  (let-values ([(defs effects) (labelify-begin '() (cdr (syntax->list stx)))])
    (if (null? defs)
        #`(begin #,@effects)
        #`(letrec (#,@defs)
            (begin #,@effects)))))

(define-syntax (new-define stx)
  (syntax-parse stx
    ;; TODO: For some reason, ~literal new-lambda doesn't work
    ;; Fix because ~datum is fragile.
    [(_ name info ((~datum lambda) (args ...) body))
     (let ([info-dict (infostx->dict #'info)])
       #`(define name
           (new-lambda (args ...)
             (do-bind-locals #,(bind-info info-dict #'body) args ...
                             #,@(get-info-bound-vars info-dict)))))]
    [(_ name ((~datum lambda) (args ...) body))
     #`(define name
         (new-lambda (args ...) (do-bind-locals body args ...)))]
    [(_ name body)
     #`(define name (new-lambda () (do-bind-locals body)))]
    [(_ name info body)
     (let ([info-dict (infostx->dict #'info)])
       #`(define name
           (new-lambda ()
             (do-bind-locals #,(bind-info info-dict #'body)
                             #,@(get-info-bound-vars info-dict)))))]))

(begin-for-syntax
  (require syntax/id-set racket/set)
  (define gathered-locals (mutable-free-id-set))

  (define (reset-locals!)
    (set-clear! gathered-locals))

  (define (collect-local! id)
    (set-add! gathered-locals id))

  (define (get-locals)
    (set->list gathered-locals)))

;; TODO: For interoperability, would be better if this was part of begin.
;; But, we need the except list... so modules and definitions need to pass it
;; down.
;; Or actually, the IRs should just change to have locals declared, and this
;; could go away.
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

(define ((return-to l saved-frame))
  (set-box! current-fvar-offset saved-frame)
  (l))

(define-syntax (return-point stx)
  (syntax-parse stx
    [(_ label tail)
     #`(let/cc l1
         (let ([label (let ([f (return-to l1 (unbox current-fvar-offset))])
                        (static-rename label (lambda () (f))))])
           tail))]))

(define (call f . ops)
  (apply f ops))

(define new-+ x64-add)
(define new-- x64-sub)
(define new-* x64-mul)

(require (for-syntax (only-in "../compiler-lib.rkt" aloc?)))

(begin-for-syntax
  (define-syntax-class addr-op
    (pattern (~or (~datum +) (~datum -))))

  (define-syntax-class not-rbp
    (pattern (~not (~literal rbp)))))

(define-syntax (set! stx)
  (syntax-parse stx
    ;; Stack pointer increment
    [(_ (~literal rbp) (binop (~literal rbp) v))
     #`(begin
         ;; NOTE: This binop gets bound to one of the new-* above.
         (inc-fvar-offset! (binop 0 v))
         (set-box! _rbp (binop rbp v)))]
    ;; Stack pointer update
    [(_ (~literal rbp) v)
     #`(set-box! _rbp v)]
    ;; Stack slot update
    [(_ ((~literal rbp) - offset) v2)
     #`(vector-set! stack (- (unbox _rbp) offset) v2)]
    [(_ v1:id v2)
     (when (aloc? (syntax->datum #'v1))
       (collect-local! #'v1))
     (syntax-parse #'v2
       [(base:not-rbp op:addr-op offset)
        #`(r:set! v1 (mref base (* (op 1 0) offset)))]
       [_
        #`(r:set! v1 v2)])]
    ;; Assign to memory
    [(_ (base:not-rbp op:addr-op offset) value)
     #`(mset! base (* (op 1 0) offset) value)]
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

(define memory (make-vector 10000 'un-aloced))
(define hbp 0)

(define (unsafe-mset! base offset value)
  (vector-set! memory (+ base offset) value))

(define (mset! base offset value)
  (let ([loc (+ base offset)])
    (when (equal? 'un-aloced (vector-ref memory loc))
      (r:error 'mset! "attempting to write to unallocated memory (base: ~a, offset: ~a)" base offset))
    (vector-set! memory loc value)))

(define (mref base offset)
  (let ([loc (+ base offset)])
    (when (equal? 'un-aloced (vector-ref memory loc))
      (r:error 'mref "attempting to read from unallocated memory (base: ~a, offset: ~a)" base offset))
    (when (equal? 'aloced (vector-ref memory loc))
      (r:error 'mref "attempting to read from uninitialized memory (base: ~a, offset: ~a)" base offset))
    (vector-ref memory loc)))

(define (alloc len)
  (let ([len8 len]
        [oldhbp hbp])
    (for ([i (in-range len)])
      (vector-set! memory (+ oldhbp i) 'alloced))
    (r:set! hbp (+ hbp len8))
    oldhbp))

(define (init-heap)
  (begin
    (r:set! hbp 0)
    (r:set! memory (make-vector 10000 'un-aloced))))

(define-syntax (new-lambda stx)
  (syntax-case stx ()
    [(_ info args tail)
     (quasisyntax/loc stx
       (new-lambda args tail))]
    [(_ args tail)
     (quasisyntax/loc stx
       (make-procedure
        (lambda args tail)
        #,(length (syntax->list #'args))
        0))]))

(define-values (procedure-label:prop procedure-label:prop? unsafe-procedure-label)
  (make-impersonator-property 'procedure-label))

(define-values (procedure-env:prop procedure-env:prop? unsafe-procedure-env)
  (make-impersonator-property 'procedure-env))

(define-values (procedure-arity:prop procedure-arity:prop? unsafe-procedure-arity)
  (make-impersonator-property 'procedure-arity))

(define (make-procedure label arity env-size)
  (impersonate-procedure label #f
                         procedure-label:prop label
                         procedure-env:prop (unsafe-make-vector env-size)
                         procedure-arity:prop arity))
(define (unsafe-procedure-ref p i)
  (unsafe-vector-ref (unsafe-procedure-env p) i))
(define (unsafe-procedure-set! p i v)
  (unsafe-vector-set! (unsafe-procedure-env p) i v))

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
     #`(let ([aloc (make-procedure (unsafe-procedure-label label) arity #,(length (syntax->datum #'(es ...))))])
         (cletrec (oths ...)
                  (begin
                    (unless (equal? arity (sub1 (unsafe-procedure-arity label)))
                      (r:error 'make-closure "arity argument doesn't match label"))
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
