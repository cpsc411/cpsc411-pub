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
 #;"v10.rkt"
 "v9.rkt"
 (only-in racket/base [vector? host:vector?])
 (for-label (only-in racket/base [vector? host:vector?])))

(provide (all-defined-out))

@define-grammar/pred[racketish-surface
#:literals (name? int61? uint8? ascii-char-literal? host:vector?)
#:datum-literals (module lambda define call let if void error * + - eq? < <= >
                  >= fixnum? boolean? empty? void? ascii-char? error? not
                  procedure? vector? pair? cons car cdr make-vector
                  vector-length vector-set! vector-ref procedure-arity
                  and or quote vector begin empty)
#;(define x value)
[p     (module (define x (lambda (x ...) value)) ... value)]

#;(letrec ([x value] ...) value)
[value triv
       (quote s-expr)

       (value value ...)
       (macro-id value ...)
       (let ([x value] ...) value)
       (if value value value)
       (call value value ...)]
[triv  fixnum prim-f x #t #f empty (void) (error uint8) ascii-char-literal
       (lambda (x ...) value)
       vec-literal]
[x     name? prim-f]
[s-expr #t #f fixnum ascii-char-literal (s-expr ...)]
[macro-id vector and or begin]
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
[vec-literal host:vector?]
]

(define (interp-racketish-surface x)
  (interp-exprs-lang-v9 x))

@define-grammar/pred[racketish-unique
#:literals (aloc? int61? uint8? ascii-char-literal? quote)
#:datum-literals (module lambda define call let if void error * + - eq? < <= >
                  >= fixnum? boolean? empty? void? ascii-char? error? not
                  procedure? vector? pair? cons car cdr make-vector
                  vector-length vector-set! vector-ref procedure-arity
                  and or quote vector begin empty)
#;(define aloc value)
[p     (module (define aloc (lambda (aloc ...) value)) ... value)]
 [value triv
        #;(unsyntax @racketvarfont{'s-expr})
        ;#(value ...)
        (quote s-expr)
        (vector value ...)
        (value value ...)
        (macro-id value ...)
        (let ([aloc value] ...) value)
        #;(letrec ([aloc value] ...) value)
        (if value value value)
        (call value value ...)]
 [triv  fixnum prim-f aloc #t #f empty
        (void) (error uint8) ascii-char-literal
        (lambda (aloc ...) value)]
 [s-expr #t #f fixnum ascii-char-literal (s-expr ...)]
 [macro-id and or quote vector begin]
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

(define (interp-racketish-unique x)
  (interp-exprs-lang-v9 x))
