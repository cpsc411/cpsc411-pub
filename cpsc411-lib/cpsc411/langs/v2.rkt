#lang at-exp racket/base

(require
 cpsc411/compiler-lib
 cpsc411/info-lib
 scribble/bettergrammar
 racket/contract
 (for-label cpsc411/compiler-lib)
 (for-label cpsc411/info-lib)
 (for-label racket/contract)
 "redex-gen.rkt"
 "v1.rkt"
 (submod "base.rkt" interp))

(provide (all-defined-out))

@define-grammar/pred[abs-asm-lang-v2
  #:literals (aloc? int64? info?)
  #:datum-literals (module halt begin set! + *)
  [p     (module info tail)]
  [info  info?]
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

(define interp-abs-asm-lang-v2 interp-base)

@define-grammar/pred[abs-asm-lang-v2/locals
  #:literals (int64? aloc? any info/c)
  #:datum-literals (module locals begin halt set! * +)
  [p     (module info tail)]
  [info  #:with-contract
         (info/c (locals (aloc ...)))
         (info/c (locals (aloc? ...)))]
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

(define interp-abs-asm-lang-v2/locals interp-base)

@define-grammar/pred[asm-lang-v2/locals
  #:literals (int64? aloc? any info/c register? fvar?)
  #:datum-literals (module locals begin halt set! * +)
  [p     (module info effect)]
  [info  #:with-contract
         (info/c (locals (aloc ...)))
         (let ([loc? (or/c register? aloc? fvar?)]
               [rloc? (or/c register? fvar?)])
           (info/c (locals (aloc? ...))))]
  [value  triv
          (binop triv triv)]
  [effect (set! loc value)
          (begin effect ... effect)]
  [triv  int64 loc]
  [loc   rloc aloc]
  [binop * +]
  [aloc  aloc?]
  [int64 int64?]
  [rloc  register? fvar?]
]

@define-grammar/pred[asm-lang-v2
  #:literals (int64? aloc? any info/c register? fvar?)
  #:datum-literals (module locals begin halt set! * +)
  [p     (module info effect)]
  [info  #:with-contract
         (info/c
           (locals (aloc ...))
           (rlocals (aloc ...))
           (assignment ((aloc rloc) ...)))
         (let ([loc? (or/c register? aloc? fvar?)]
               [rloc? (or/c register? fvar?)])
           (info/c
            (locals (aloc? ...))
            (rlocals (aloc? ...))
            (assignment ((aloc? rloc?) ...))))]
  [value  triv
          (binop triv triv)]
  [effect (set! loc value)
          (begin effect ... effect)]
  [triv  int64 loc]
  [loc   rloc aloc]
  [binop * +]
  [aloc  aloc?]
  [int64 int64?]
  [rloc  register? fvar?]
]

(define interp-asm-lang-v2 interp-base)

@define-grammar/pred[nested-asm-lang-v2
  #:literals (int64? fvar? int32?)
  #:datum-literals (module set! begin rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10
                           r11 r12 r13 r14 r15 * + halt)
  [p     effect]
  [effect (set! fvar int32)
          (set! fvar reg)
          (set! reg loc)
          (set! reg triv)
          (set! reg_1 (binop reg_1 int32))
          (set! reg_1 (binop reg_1 loc))
          (begin effect ... effect)]
  [triv  int64 loc]
  [loc   reg fvar]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * +]
  [int64 int64?]
  [int32 int32?]
  [fvar  fvar?]
]

(define interp-nested-asm-lang-v2 interp-base)

@define-grammar/pred[paren-x64-fvars-v2
  #:literals (int64? int32? fvar?)
  #:datum-literals (set! begin rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10
                           r11 r12 r13 r14 r15 * +)
  [p     (begin s ...)]
  [s     (set! fvar int32)
         (set! fvar reg)
         (set! reg loc)
         (set! reg triv)
         (set! reg_1 (binop reg_1 int32))
         (set! reg_1 (binop reg_1 loc))]
  [triv  reg int64]
  [loc   reg fvar]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [binop * +]
  [int64 int64?]
  [int32 int32?]
  [fvar fvar?]
]

(define interp-paren-x64-fvars-v2 interp-paren-x64-v1)

@define-grammar/pred[paren-x64-v2
  #:literals (int64? int32? dispoffset? frame-base-pointer-register?)
  #:datum-literals (set! begin rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12
  r13 r14 r15 * +)
  [p     (begin s ...)]
  [s     (set! addr int32)
         (set! addr reg)
         (set! reg loc)
         (set! reg triv)
         (set! reg_1 (binop reg_1 int32))
         (set! reg_1 (binop reg_1 loc))]
  [triv  reg int64]
  [loc   reg addr]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [binop * +]
  [int64 int64?]
  [int32 int32?]
  [dispoffset dispoffset?]
]

(define interp-paren-x64-v2 interp-paren-x64-v1)
