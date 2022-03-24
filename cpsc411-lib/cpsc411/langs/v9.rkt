#lang at-exp racket/base

(require
 cpsc411/compiler-lib
 cpsc411/info-lib
 scribble/bettergrammar
 racket/contract
 (for-label cpsc411/info-lib)
 (for-label racket/contract)
 (for-label cpsc411/compiler-lib)
 "redex-gen.rkt"
 "v8.rkt")

(provide (all-defined-out))

@define-grammar/pred[exprs-lang-v9
#:literals (name? int61? uint8? ascii-char-literal?)
#:datum-literals (module lambda define call let if void error * + - eq? < <= >
                         >= fixnum? boolean? empty? void? ascii-char? error? not
                         procedure? vector? pair? cons car cdr make-vector
                         vector-length vector-set! vector-ref procedure-arity)
[p     (module (define x (lambda (x ...) value)) ... value)]
[value triv
       (let ([x value] ...) value)
       (if value value value)
       (call value value ...)]
[triv  x fixnum #t #f empty (void) (error uint8) ascii-char-literal
      (lambda (x ...) value)]
[x     name? prim-f]
[prim-f * + - < <= > >= eq?
        fixnum? boolean? empty? void? ascii-char? error? not
        pair?
        procedure?
        vector?

        cons
        car
        cdr

        make-vector
        vector-length
        vector-set!
        vector-ref

        procedure-arity]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]

(define (interp-exprs-lang-v9 x)
  (interp-exprs-lang-v8 x))

@define-grammar/pred[exprs-unique-lang-v9
#:literals (aloc? label? int64? int61? uint8? ascii-char-literal?)
#:datum-literals (module lambda define call let if void error * + - eq? < <= >
                         >= fixnum? boolean? empty? void? ascii-char? error? not

                         pair? vector? cons car cdr make-vector vector-length vector-set! vector-ref)
[p     (module (define aloc (lambda (aloc ...) value)) ... value)]
[value triv
       (call value value ...)
       (let ([aloc value] ...) value)
       (if value value value)]
[triv  aloc prim-f fixnum #t #f empty (void) (error uint8)
       ascii-char-literal
       (lambda (aloc ...) value)]
[prim-f * + - eq? < <= > >=
        fixnum? boolean? empty? void? ascii-char? error? not
        pair?
        procedure?
        vector?

        cons
        car
        cdr

        make-vector
        vector-length
        vector-set!
        vector-ref

        procedure-arity]
[aloc aloc?]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]

@define-grammar/pred[exprs-unsafe-data-lang-v9
#:literals (aloc? label? int64? int61? uint8? ascii-char-literal?)
#:datum-literals (module lambda define call let if begin void error
                         true false
                         >= fixnum? boolean? empty? void? ascii-char? error?
                         unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
                         unsafe-fx>=

                         not pair? vector? procedure?

                         cons unsafe-car unsafe-cdr

                         unsafe-make-vector
                         unsafe-vector-length
                         unsafe-vector-set!
                         unsafe-vector-ref
                         unsafe-procedure-arity)
[p     (module (define aloc (lambda (aloc ...) value)) ... value)]
[pred  value
       (true)
       (false)
       (not pred)
       (let ([aloc value] ...) pred)
       (if pred pred pred)]
[value     triv
       (primop value ...)
       (call value value ...)
       (let ([aloc value] ...) value)
       (if pred value value)
       (begin effect ... value)]
[effect (primop value ...) (begin effect ... effect)]
[triv    aloc fixnum #t #f empty (void) (error uint8) ascii-char-literal
         (lambda (aloc ...) value)]
[primop  unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
         unsafe-fx>=

         fixnum? boolean? empty? void? ascii-char? error? not
         pair? vector? procedure?

         cons unsafe-car unsafe-cdr

         unsafe-make-vector
         unsafe-vector-length
         unsafe-vector-set!
         unsafe-vector-ref

         unsafe-procedure-arity]
[aloc aloc?]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]

@define-grammar/pred[exprs-unsafe-lang-v9
#:literals (aloc? label? int64? int61? uint8? ascii-char-literal?)
#:datum-literals (letrec module lambda define call let if begin void error
                         >= fixnum? boolean? empty? void? ascii-char? error?
                         unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
                         unsafe-fx>=

                         not pair? vector? procedure?

                         cons unsafe-car unsafe-cdr

                         unsafe-make-vector
                         unsafe-vector-length
                         unsafe-vector-set!
                         unsafe-vector-ref
                         unsafe-procedure-arity
                         unsafe-procedure-call)
[p     (module (define aloc (lambda (aloc ...) value)) ... value)]
[pred  value
       (true)
       (false)
       (not pred)
       (let ([aloc value] ...) pred)
       (if pred pred pred)]
[value triv
       (primop value ...)
       (unsafe-procedure-call value value ...)
       (let ([aloc value] ...) value)
       (if pred value value)
       (begin effect ... value)]
[effect (primop value ...) (begin effect ... effect)]
[triv   aloc fixnum #t #f empty (void) (error uint8)
        ascii-char-literal (lambda (aloc ...) value)]
[primop  unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
         unsafe-fx>=

         fixnum? boolean? empty? void? ascii-char? error? not
         pair? vector? procedure?

         cons unsafe-car unsafe-cdr

         unsafe-make-vector
         unsafe-vector-length
         unsafe-vector-set!
         unsafe-vector-ref

         unsafe-procedure-arity]
[aloc aloc?]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]


@define-grammar/pred[just-exprs-lang-v9
#:literals (aloc? label? int64? int61? uint8? ascii-char-literal?)
#:datum-literals (letrec module lambda define call let if begin void error
                         >= fixnum? boolean? empty? void? ascii-char? error?
                         unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
                         unsafe-fx>=

                         not pair? vector? procedure?

                         cons unsafe-car unsafe-cdr

                         unsafe-make-vector
                         unsafe-vector-length
                         unsafe-vector-set!
                         unsafe-vector-ref
                         unsafe-procedure-arity
                         unsafe-procedure-call)
[p     (module e)]
[pred  e
       (true)
       (false)
       (not pred)
       (let ([aloc e] ...) pred)
       (if pred pred pred)]
[e     v
       (primop e ...)
       (unsafe-procedure-call e e ...)
       (letrec ([aloc (lambda (aloc ...) e)] ...) e)
       (let ([aloc e] ...) e)
       (if pred e e)
       (begin effect ... e)]
[effect (primop e ...) (begin effect ... effect)]
[v     aloc fixnum #t #f empty (void) (error uint8)
       ascii-char-literal
       (lambda (aloc ...) e)]
[primop  unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
         unsafe-fx>=

         fixnum? boolean? empty? void? ascii-char? error? not
         pair? vector? procedure?

         cons unsafe-car unsafe-cdr

         unsafe-make-vector
         unsafe-vector-length
         unsafe-vector-set!
         unsafe-vector-ref

         unsafe-procedure-arity]
[aloc aloc?]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]

@define-grammar/pred[lam-opticon-lang-v9
#:literals (aloc? label? int64? int61? uint8? ascii-char-literal?)
#:datum-literals (letrec module lambda define call let if begin void error
                         >= fixnum? boolean? empty? void? ascii-char? error?
                         unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
                         unsafe-fx>=

                         not pair? vector? procedure?

                         cons unsafe-car unsafe-cdr

                         unsafe-make-vector
                         unsafe-vector-length
                         unsafe-vector-set!
                         unsafe-vector-ref
                         unsafe-procedure-arity
                         unsafe-procedure-call)
[p     (module e)]
[pred  e
       (true)
       (false)
       (not pred)
       (let ([aloc e] ...) pred)
       (if pred pred pred)]
[e     v
       (primop e ...)
       (unsafe-procedure-call e e ...)
       (letrec ([aloc (lambda (aloc ...) e)] ...) e)
       (let ([aloc e] ...) e)
       (if pred e e)
       (begin effect ... e)]
[effect (primop e ...) (begin effect ... effect)]
[v     aloc fixnum #t #f empty (void) (error uint8)
       ascii-char-literal]
[primop  unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
         unsafe-fx>=

         fixnum? boolean? empty? void? ascii-char? error? not
         pair? vector? procedure?

         cons unsafe-car unsafe-cdr

         unsafe-make-vector
         unsafe-vector-length
         unsafe-vector-set!
         unsafe-vector-ref

         unsafe-procedure-arity]
[aloc aloc?]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]

@define-grammar/pred[lam-free-lang-v9
#:literals (aloc? label? int64? int61? uint8? ascii-char-literal?)
#:datum-literals (free letrec module lambda define call let if begin void error
                         >= fixnum? boolean? empty? void? ascii-char? error?
                         unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
                         unsafe-fx>=

                         not pair? vector? procedure?

                         cons unsafe-car unsafe-cdr

                         unsafe-make-vector
                         unsafe-vector-length
                         unsafe-vector-set!
                         unsafe-vector-ref
                         unsafe-procedure-arity
                         unsafe-procedure-call)
[p     (module e)]
[info  ((free (aloc ...)) any ...)]
[pred  e
       (true)
       (false)
       (not pred)
       (let ([aloc e] ...) pred)
       (if pred pred pred)]
[e     v
       (primop e ...)
       (unsafe-procedure-call e e ...)
       (letrec ([aloc (lambda info (aloc ...) e)] ...) e)
       (let ([aloc e] ...) e)
       (if pred e e)
       (begin effect ... e)]
[effect (primop e ...) (begin effect ... effect)]
[v     aloc fixnum #t #f empty (void) (error uint8)
       ascii-char-literal]
[primop  unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
         unsafe-fx>=

         fixnum? boolean? empty? void? ascii-char? error? not
         pair? vector? procedure?

         cons unsafe-car unsafe-cdr

         unsafe-make-vector
         unsafe-vector-length
         unsafe-vector-set!
         unsafe-vector-ref

         unsafe-procedure-arity]
[aloc aloc?]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]

@define-grammar/pred[closure-lang-v9
#:literals (aloc? label? int64? int61? uint8? ascii-char-literal?)
#:datum-literals (closure-ref closure-call make-closure cletrec
                              letrec module lambda define call let if
                              begin void error
                         >= fixnum? boolean? empty? void? ascii-char? error?
                         unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
                         unsafe-fx>=

                         not pair? vector? procedure?

                         cons unsafe-car unsafe-cdr

                         unsafe-make-vector
                         unsafe-vector-length
                         unsafe-vector-set!
                         unsafe-vector-ref
                         unsafe-procedure-arity)
[p     (module e)]
[pred  e
       (true)
       (false)
       (not pred)
       (let ([aloc e] ...) pred)
       (if pred pred pred)]
[e     v
       (primop e ...)
       (closure-ref e e)
       (closure-call e e ...)
       (call e e ...)
       (letrec ([label (lambda (aloc ...) e)] ...) e)
       (cletrec ([aloc (make-closure label e ...)] ...) e)
       (let ([aloc e] ...) e)
       (if pred e e)
       (begin effect ... e)]
[effect (primop e ...) (begin effect ... effect)]
[v     label aloc fixnum #t #f empty (void) (error uint8)
       ascii-char-literal]
[primop  unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
         unsafe-fx>=

         fixnum? boolean? empty? void? ascii-char? error? not
         pair? vector? procedure?

         cons unsafe-car unsafe-cdr

         unsafe-make-vector
         unsafe-vector-length
         unsafe-vector-set!
         unsafe-vector-ref

         unsafe-procedure-arity]
[aloc aloc?]
[label label?]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]


@define-grammar/pred[hoisted-lang-v9
#:literals (aloc? label? int64? int61? uint8? ascii-char-literal?)
#:datum-literals (closure-ref closure-call make-closure cletrec
                              letrec module lambda define call let if
                              begin void error
                         >= fixnum? boolean? empty? void? ascii-char? error?
                         unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
                         unsafe-fx>=

                         not pair? vector? procedure?

                         cons unsafe-car unsafe-cdr

                         unsafe-make-vector
                         unsafe-vector-length
                         unsafe-vector-set!
                         unsafe-vector-ref
                         unsafe-procedure-arity)
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[pred  e
       (true)
       (false)
       (not pred)
       (let ([aloc e] ...) pred)
       (if pred pred pred)]
[e     v
       (primop e ...)
       (closure-ref e e)
       (closure-call e e ...)
       (call e e ...)
       (cletrec ([aloc (make-closure label e ...)] ...) e)
       (let ([aloc e] ...) e)
       (if pred e e)
       (begin effect ... e)]
[effect (primop e ...) (begin effect ... effect)]
[v     label aloc fixnum #t #f empty (void) (error uint8)
       ascii-char-literal]
[primop  unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
         unsafe-fx>=

         fixnum? boolean? empty? void? ascii-char? error? not
         pair? vector? procedure?

         cons unsafe-car unsafe-cdr

         unsafe-make-vector
         unsafe-vector-length
         unsafe-vector-set!
         unsafe-vector-ref

         unsafe-procedure-arity]
[aloc aloc?]
[label label?]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]

@define-grammar/pred[proc-exposed-lang-v9
#:literals (aloc? label? int64? int61? uint8? ascii-char-literal?)
#:datum-literals (letrec module lambda define call let if begin void error
                         >= fixnum? boolean? empty? void? ascii-char? error?
                         unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
                         unsafe-fx>=

                         not pair? vector? procedure?

                         cons unsafe-car unsafe-cdr

                         unsafe-make-vector
                         unsafe-vector-length
                         unsafe-vector-set!
                         unsafe-vector-ref

                         make-procedure
                         unsafe-procedure-arity
                         unsafe-procedure-label
                         unsafe-procedure-ref
                         unsafe-procedure-set!)
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[pred  e
       (true)
       (false)
       (not pred)
       (let ([aloc e] ...) pred)
       (if pred pred pred)]
[e     v
       (primop e ...)
       (call e e ...)
       (let ([aloc e] ...) e)
       (if pred e e)
       (begin effect ... e)]
[effect (primop e ...) (begin effect ... effect)]
[v     label aloc fixnum #t #f empty (void) (error uint8)
       ascii-char-literal]
[primop  unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
         unsafe-fx>=

         fixnum? boolean? empty? void? ascii-char? error? not
         pair? vector? procedure?

         cons unsafe-car unsafe-cdr

         unsafe-make-vector
         unsafe-vector-length
         unsafe-vector-set!
         unsafe-vector-ref

         make-procedure
         unsafe-procedure-arity
         unsafe-procedure-label
         unsafe-procedure-ref
         unsafe-procedure-set!]
[aloc aloc?]
[label label?]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]
