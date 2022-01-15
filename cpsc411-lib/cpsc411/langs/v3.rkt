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
 (submod "base.rkt" interp))

(provide (all-defined-out))

@define-grammar/pred[values-lang-v3
  #:literals (int64? name?)
  #:datum-literals (let * +)
  [p (module tail)]
  [tail  value (let ([x value] ...) tail)]
  [value triv
         (binop triv triv)
         (let ([x value] ...) value)]
  [triv int64 x]
  [x name?]
  [binop * +]
  [int64 int64?]
]

(define interp-values-lang-v3 interp-base)

@define-grammar/pred[values-unique-lang-v3
  #:literals (int64? aloc?)
  #:datum-literals (module let * +)
  [p     (module tail)]
  [tail  value (let ([aloc value] ...) tail)]
  [value triv
         (binop triv triv)
         (let ([aloc value] ...) value)]
  [triv  int64 aloc]
  [binop * +]
  [aloc aloc?]
  [int64 int64?]
]

(define interp-values-unique-lang-v3 interp-base)

@define-grammar/pred[imp-mf-lang-v3
  #:literals (int64? aloc?)
  #:datum-literals (module set! begin * +)
  [p      (module tail)]
  [tail   value
          (begin effect ... tail)]
  [value  triv
          (binop triv triv)
          (begin effect ... value)]
  [effect (set! aloc value)
          (begin effect ... effect)]
  [triv   int64 aloc]
  [binop  * +]
  [aloc   aloc?]
  [int64 int64?]
]

(define interp-imp-mf-lang-v3 interp-base)

@define-grammar/pred[imp-cmf-lang-v3
  #:literals (int64? aloc?)
  #:datum-literals (module set! begin * +)
  [p      (module tail)]
  [tail   value
          (begin effect ... tail)]
  [value  triv
          (binop triv triv)]
  [effect (set! aloc value)
          (begin effect ... effect)]
  [triv   int64 aloc]
  [binop  * +]
  [aloc   aloc?]
  [int64 int64?]
]

(define interp-imp-cmf-lang-v3 interp-base)
