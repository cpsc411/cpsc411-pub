#lang at-exp racket/base

(require
 cpsc411/compiler-lib
 cpsc411/info-lib
 scribble/bettergrammar
 racket/contract
 (for-label cpsc411/info-lib)
 (for-label racket/contract)
 (for-label cpsc411/compiler-lib)
 (submod "base.rkt" interp)
 "redex-gen.rkt"
 "v9.rkt")

(provide (all-defined-out))

@define-grammar/pred[racketish-core-v10
#:literals (name? int61? uint8? ascii-char-literal?)
#:datum-literals (module lambda define call let if void error * + - eq? < <= >
                         >= fixnum? boolean? empty? void? ascii-char? error? not
                         procedure? vector? pair? cons car cdr make-vector
                         vector-length vector-set! vector-ref procedure-arity)
[p     (module (define x value) ... value)]
[value triv
       (call value value ...)
       (let ([x value] ...) value)
       (letrec ([x value] ...) value)
       (if value value value)]
[triv  x fixnum #t #f () (void) (error uint8) ascii-char-literal (lambda (x ...) value)]
[x     name? prim-f]
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
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]

(define (interp-racketish-core-v10 x)
  (interp-exprs-lang-v9 x))

@define-grammar/pred[racketish-unique-v10
#:literals (int61? uint8? ascii-char-literal? aloc?)
#:datum-literals (module lambda define call let if void error * + - eq? < <= >
                         >= fixnum? boolean? empty? void? ascii-char? error? not
                         procedure? vector? pair? cons car cdr make-vector
                         vector-length vector-set! vector-ref procedure-arity)
[p     (module (define aloc value) ... value)]
[value triv
       (call value value ...)
       (let ([aloc value] ...) value)
       (letrec ([alocx value] ...) value)
       (if value value value)]
[triv  aloc prim-f fixnum #t #f () (void) (error uint8) ascii-char-literal (lambda (x ...) value)]
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

(define (interp-racketish-unique-v10 x)
  (interp-racketish-core-v10 x))

@define-grammar/pred[assigned-lang-v10
#:literals (int61? uint8? ascii-char-literal? aloc?)
#:datum-literals (module lambda define call let if void error * + - eq? < <= >
                  >= fixnum? boolean? empty? void? ascii-char? error? not
                  procedure? vector? pair? cons car cdr make-vector
                  vector-length vector-set! vector-ref procedure-arity)

[p     (module value)]
[effect (set! aloc value)]
[value triv
       (call value value ...)
       (letrec ([aloc value] ...) value)
       (let (info ((assigned (aloc ...)))) ([aloc value] ...) value)
       (if value value value)
       (begin effect ... value)]
[triv  fixnum prim-f aloc #t #f ()
       (void) (error uint8) ascii-char-literal
       (lambda (aloc ...) e)]
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
]

@define-grammar/pred[exprs-safe-lang-v10
#:literals (int61? uint8? ascii-char-literal? aloc?)
#:datum-literals (module lambda define call let if void error * + - eq? < <= >
                  >= fixnum? boolean? empty? void? ascii-char? error? not
                  procedure? vector? pair? cons car cdr make-vector
                  vector-length vector-set! vector-ref procedure-arity)
[p     (module (define aloc value) ... value)]
[value triv
       (call value value ...)
       (let ([aloc value] ...) value)
       (letrec ([aloc value] ...) value)
       (if value value value)]
[triv  aloc prim-f fixnum #t #f () (void) (error uint8) ascii-char-literal (lambda (x ...) value)]
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

(define (interp-exprs-safe-lang-v10 x)
  (interp-racketish-core-v10 x))
