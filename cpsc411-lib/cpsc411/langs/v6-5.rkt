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
 (submod "base.rkt" interp)
 "v6.rkt")

(provide (all-defined-out))

@define-grammar/pred[exprs-lang-v6.5
#:literals (name? int64?)
#:datum-literals (module lambda define let if void error * + - = < <= >
                  >= call not)
[p     (module (define x (lambda (x ...) value)) ... value)]
[pred  (relop triv triv)
       (true)
       (false)
       (not pred)
       (let ([x value] ...) pred)
       (if pred pred pred)]
[value triv
       (binop value value)
       (let ([x value] ...) value)
       (if pred value value)
       (call x value ...)]
[triv  int64 x]
[x     name?]
[binop * + -]
[relop < <= = >= > !=]
[int64 int64?]
]

(define (interp-exprs-lang-v6.5 x)
  (interp-base x))

@define-grammar/pred[exprs-unique-lang-v6.5
#:literals (aloc? int64? label?)
#:datum-literals (module lambda define let if void error * + - = < <= >
                         >= not call)
[p     (module (define label (lambda (aloc ...) value)) ... value)]
[pred  (relop opand opand)
       (true)
       (false)
       (not pred)
       (let ([aloc value] ...) pred)
       (if pred pred pred)]
[value triv
       (binop value value)
       (call triv value ...)
       (let ([aloc value] ...) value)
       (if pred value value)]
[opand int64 aloc]
[triv  opand label]
[binop * + -]
[relop < <= = >= > !=]
[aloc  aloc?]
[label label?]
[int64 int64?]
]

(define (interp-exprs-unique-lang-v6.5 x)
  (interp-base x))

@define-grammar/pred[exprs-unique-lang-v6.5/context
#:literals (aloc? label? int64?)
#:datum-literals (module lambda define apply let if void error * + - = != < <= >
                  >= true false not call)
[p     (module (define label (lambda (aloc ...) tail)) ... tail)]
[pred  (relop opand opand)
       (true)
       (false)
       (not pred)
       (let ([aloc value] ...) pred)
       (if pred pred pred)]
[tail  value
       (let ([aloc value] ...) tail)
       (if pred tail tail)
       (call triv value ...)]
[value triv
       (binop value value)
       (let ([aloc value] ...) value)
       (if pred value value)
       (call triv value ...)]
[opand int64 aloc]
[triv opand label]
[binop * + -]
[relop < <= = >= > !=]
[aloc  aloc?]
[label label?]
[int64 int64?]
]

(define (interp-exprs-unique-lang-v6.5/context x)
  (interp-base x))
