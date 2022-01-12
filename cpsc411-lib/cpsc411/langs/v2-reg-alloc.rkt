#lang at-exp racket/base

(require
 cpsc411/compiler-lib
 cpsc411/info-lib
 scribble/bettergrammar
 racket/contract
 (for-label cpsc411/info-lib)
 (for-label racket/contract)
 (for-label cpsc411/compiler-lib)
 "../utils/redex-gen.rkt")

(provide (all-defined-out))

@define-grammar/pred[asm-lang-v2/undead
  #:literals (int64? aloc? info/c undead-set-tree?)
  #:datum-literals (module begin locals undead-out set! * + halt)
  [p     (module info tail)]
  [info  #:with-contract
         (info/c
          (locals (aloc ...))
          (undead-out undead-set-tree?))
         (info/c
          (locals (aloc? ...))
          (undead-out undead-set-tree?))]
  [tail  (halt triv)
         (begin effect ... tail)]
  [effect (set! aloc triv)
          (set! aloc_1 (binop aloc_1 triv))
          (begin effect ... effect)]
  [triv  int64 aloc]
  [binop * +]
  [aloc  aloc?]
  [int64 int64?]
]

@define-grammar/pred[asm-lang-v2/conflicts
  #:literals (int64? aloc?)
  #:datum-literals (module begin locals conflicts undead-out set! * + halt)
  [p     (module info tail)]
  [info  #:with-contract
         (info/c
          (locals (aloc ...))
          (conflicts ((aloc (aloc ...)) ...)))
         (info/c
          (locals (aloc? ...))
          (conflicts ((aloc? (aloc? ...)) ...)))]
  [tail  (halt triv)
         (begin effect ... tail)]
  [effect (set! aloc triv)
          (set! aloc_1 (binop aloc_1 triv))
          (begin effect ... effect)]
  [triv  int64 aloc]
  [binop * +]
  [aloc  aloc?]
  [int64 int64?]
]
