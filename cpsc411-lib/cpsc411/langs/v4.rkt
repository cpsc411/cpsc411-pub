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
 "v3.rkt"
 (submod "base.rkt" interp))

(provide (all-defined-out))

@define-grammar/pred[values-lang-v4
  #:literals (name? int64?)
  #:datum-literals (module let true false not if * + < <= = >= > !=)
  [p     (module tail)]
  [pred  (relop triv triv)
         (true)
         (false)
         (not pred)
         (let ([x value] ...) pred)
         (if pred pred pred)]
  [tail  value
         (let ([x value] ...) tail)
         (if pred tail tail)]
  [value triv
         (binop triv triv)
         (let ([x value] ...) value)
         (if pred value value)]
  [triv int64 x]
  [x     name?]
  [binop * +]
  [relop < <= = >= > !=]
  [int64 int64?]
]

(define (interp-values-lang-v4 x)
  (interp-values-lang-v3 x))

@define-grammar/pred[values-unique-lang-v4
  #:literals (int64? aloc?)
  #:datum-literals (module let true false not if * + < <= = >= > !=)
  [p     (module tail)]
  [pred  (relop triv triv)
         (true)
         (false)
         (not pred)
         (let ([aloc value] ...) pred)
         (if pred pred pred)]
  [tail  value
         (let ([aloc value] ...) tail)
         (if pred tail tail)]
  [value triv
         (binop triv triv)
         (let ([aloc value] ...) value)
         (if pred value value)]
  [triv int64 aloc]
  [binop * +]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
]

(define (interp-values-unique-lang-v4 x)
  (interp-values-lang-v4 x))

@define-grammar/pred[imp-mf-lang-v4
  #:literals (int64? aloc? any)
  #:datum-literals (module true false not begin if set! * + < <= = >= > !=)
  [p      (module tail)]
  [pred   (relop triv triv)
          (true)
          (false)
          (not pred)
          (begin effect ... pred)
          (if pred pred pred)]
  [tail   value
          (begin effect ... tail)
          (if pred tail tail)]
  [value  triv
          (binop triv triv)
          (if pred value value)
          (begin effect ... value)]
  [effect #;(nop)
          (set! aloc value)
          (if pred effect effect)
          (begin effect ... effect)]
  [triv   int64 aloc]
  [binop  * +]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
]

(define (interp-imp-mf-lang-v4 x)
  (interp-values-unique-lang-v4 x))

@define-grammar/pred[imp-cmf-lang-v4
  #:literals (int64? aloc? any)
  #:datum-literals (module true false not begin if set! * + < <= = >= > !=)
  [p      (module tail)]
  [pred   (relop triv triv)
          (true)
          (false)
          (not pred)
          (begin effect ... pred)
          (if pred pred pred)]
  [tail   value
          (begin effect ... tail)
          (if pred tail tail)]
  [value  triv
          (binop triv triv)]
  [effect #;(nop)
          (set! aloc value)
          (begin effect ... effect)
          (if pred effect effect)]
  [triv   int64 aloc]
  [binop  * +]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
]

(define (interp-imp-cmf-lang-v4 x)
  (interp-imp-mf-lang-v4 x))

@define-grammar/pred[asm-pred-lang-v4
  #:literals (int64? aloc? any info?)
  #:datum-literals (module true false not begin if set! * + < <= = >= > != halt nop)
  [p    (module info tail)]
  [info info?]
  [pred (relop aloc triv)
        (true)
        (false)
        (not pred)
        (begin effect ... pred)
        (if pred pred pred)]
  [tail (halt triv)
        (begin effect ... tail)
        (if pred tail tail)]
  [effect #;(nop)
          (set! aloc triv)
          (set! aloc_1 (binop aloc_1 triv))
          (begin effect ... effect)
          (if pred effect effect)]
  [triv int64 aloc]
  [binop * +]
  [relop < <= = >= > !=]
  [int64  int64?]
  [aloc aloc?]
]

(define (interp-asm-pred-lang-v4 x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v4/locals
  #:literals (int64? aloc? any info? info/c)
  #:datum-literals (module locals true false not begin if set! * + < <= = >= >
                           != halt)
  [p    (module info tail)]
  [info ((locals (aloc ...)) any ...)]
  [pred (relop aloc triv)
        (true)
        (false)
        (not pred)
        (begin effect ... pred)
        (if pred pred pred)]
  [tail (halt triv)
        (begin effect ... tail)
        (if pred tail tail)]
  [effect (set! aloc triv)
          (set! aloc_1 (binop aloc_1 triv))
          (begin effect ... effect)
          (if pred effect effect)]
  [triv int64 aloc]
  [binop * +]
  [relop < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
]

(define (interp-asm-pred-lang-v4/locals x)
  (interp-asm-pred-lang-v4 x))

@define-grammar/pred[asm-pred-lang-v4/undead
  #:literals (int64? aloc? info? any undead-set-tree? info/c)
  #:datum-literals (module undead-out locals true false not begin if set! * + <
                           <= = >= > != halt)
  [p    (module info tail)]
  [info #:with-contract
        (info/c
         (locals (aloc ...))
         (undead-out undead-set-tree?))
        (info/c
         (locals (aloc? ...))
         (undead-out undead-set-tree?))]
  [pred  (relop aloc triv)
         (true)
         (false)
         (not pred)
         (begin effect ... pred)
         (if pred pred pred)]
  [tail (halt triv)
        (begin effect ... tail)
        (if pred tail tail)]
  [effect (set! aloc triv)
          (set! aloc_1 (binop aloc_1 triv))
          (begin effect ... effect)
          (if pred effect effect)]
  [triv int64 aloc]
  [binop * +]
  [relop < <= = >= > !=]
  [int64  int64?]
  [aloc aloc?]
]

(define (interp-asm-pred-lang-v4/undead x)
  (interp-asm-pred-lang-v4/locals x))

@define-grammar/pred[asm-pred-lang-v4/conflicts
  #:literals (int64? aloc? info? any undead-set-tree? info/c)
  #:datum-literals (module conflicts locals true false not begin if set! * + <
                           <= = >= > != halt)
  [p    (module info tail)]
  [info #:with-contract
        (info/c
         (locals (aloc ...))
         (conflicts ((aloc (aloc ...)) ...)))
        (info/c
         (locals (aloc? ...))
         (conflicts ((aloc? (aloc? ...)) ...)))]
  [pred (relop aloc triv)
        (true)
        (false)
        (not pred)
        (begin effect ... pred)
        (if pred pred pred)]
  [tail (halt triv)
        (begin effect ... tail)
        (if pred tail tail)]
  [effect (set! aloc triv)
          (set! aloc_1 (binop aloc_1 triv))
          (begin effect ... effect)
          (if pred effect effect)]
  [triv int64 aloc]
  [binop * +]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc  aloc?]
]

(define (interp-asm-pred-lang-v4/conflicts x)
  (interp-asm-pred-lang-v4/undead x))

@define-grammar/pred[asm-pred-lang-v4/assignments
  #:literals (int64? aloc? info? any undead-set-tree? fvar? info/c)
  #:datum-literals (module assignment locals true false not begin if set! halt *
                           + < <= = >= > !=
                           rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15)
  [p    (module info tail)]
  [info #:with-contract
        (info/c
         (locals (aloc ...))
         (assignment ((aloc loc) ...)))
        (let ([loc?  (or/c register? fvar?)])
          (info/c
           (locals (aloc? ...))
           (assignment ((aloc? loc?) ...))))]
  [pred (relop aloc triv)
        (true)
        (false)
        (not pred)
        (begin effect ... pred)
        (if pred pred pred)]
  [tail (halt triv)
        (begin effect ... tail)
        (if pred tail tail)]
  [effect (set! aloc triv)
          (set! aloc_1 (binop aloc_1 triv))
          (begin effect ... effect)
          (if pred effect effect)]
  [triv int64 aloc]
  [rloc reg fvar]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * +]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [fvar fvar?]
]

(define (interp-asm-pred-lang-v4/assignments x)
  (interp-asm-pred-lang-v4/conflicts x))

@define-grammar/pred[nested-asm-lang-v4
  #:literals (int64? aloc? any fvar?)
  #:datum-literals (module true false not begin if set! * + < <= = >= > rsp rbp
                           rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14
                           r15 halt !=)
  [p     (module tail)]
  [pred  (relop loc triv)
         (true)
         (false)
         (not pred)
         (begin effect ... pred)
         (if pred pred pred)]
  [tail  (halt triv)
         (begin effect ... tail)
         (if pred tail tail)]
  [effect (set! loc triv)
          (set! loc_1 (binop loc_1 triv))
          (begin effect ... effect)
          (if pred effect effect)]
  [triv  int64 loc]
  [loc   reg fvar]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * +]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [fvar fvar?]
]

(define (interp-nested-asm-lang-v4 x)
  (interp-base x))

@;todo{Loc or ploc?}

@define-grammar/pred[block-pred-lang-v4
  #:literals (int64? aloc? info? any fvar? label?)
  #:datum-literals (module true false not begin if set! * + < <= = >= > != rsp rbp
                           rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14
                           r15 jump define halt)
  [p     (module b ... b)]
  [b     (define label tail)]
  [pred  (relop loc opand)
         (true)
         (false)
         (not pred)]
  [tail  (halt opand)
         (jump trg)
         (begin s ... tail)
         (if pred (jump trg) (jump trg))]
  [s     (set! loc triv)
         (set! loc_1 (binop loc_1 opand))]
  [triv  opand label]
  [opand int64 loc]
  [trg   label loc]
  [loc   reg fvar]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * +]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [fvar fvar?]
  [label label?]
]

(define (interp-block-pred-lang-v4 x)
  (interp-base x))

@define-grammar/pred[block-asm-lang-v4
  #:literals (int64? aloc? info? any fvar? label?)
  #:datum-literals (module true false not begin if set! * + < <= = >= > != halt
                           jump rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13
                           r14 r15 define)
  [p     (module b ... b)]
  [b     (define label tail)]
  [tail  (halt opand)
         (jump trg)
         (begin s ... tail)
         (if (relop loc opand) (jump trg) (jump trg))]
  [s     (set! loc triv)
         (set! loc_1 (binop loc_1 opand))]
  [triv  opand label]
  [opand int64 loc]
  [trg   label loc]
  [loc   reg fvar]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * +]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [fvar fvar?]
  [label label?]
]

(define (interp-block-asm-lang-v4 x)
  (interp-block-pred-lang-v4 x))

@;todo{The names "b" is bad. "b" is a definition}

@define-grammar/pred[para-asm-lang-v4
  #:literals (int64? aloc? info? any fvar? label?)
  #:datum-literals (halt true false not begin if set! * + < <= = >= > !=
                         with-label jump compare jump-if rsp rbp rax rbx rcx rdx
                         rsi rdi r8 r9 r12 r13 r14 r15)
  [p     (begin s ...)]
  [s     (halt opand)
         (set! loc triv)
         (set! loc_1 (binop loc_1 opand))
         (jump trg)
         (with-label label s)
         (compare loc opand)
         (jump-if relop trg)]
  [triv  opand label]
  [opand int64 loc]
  [trg   label loc]
  [loc   reg fvar]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * +]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [fvar fvar?]
  [label label?]
]

(define (interp-para-asm-lang-v4 x)
  (interp-base `(module ,x)))

@define-grammar/pred[paren-x64-fvars-v4
  #:literals (int64? aloc? info? any fvar? label? int32?)
  #:datum-literals (module true false not begin if set! * + < <= = >= > !=
                           with-label jump compare jump-if
                           rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13
                           r14 r15)
  [p     (begin s ...)]
  [s     (set! fvar int32)
         (set! fvar trg)
         (set! reg loc)
         (set! reg triv)
         (set! reg_1 (binop reg_1 int32))
         (set! reg_1 (binop reg_1 loc))
         (with-label label s)
         (jump trg)
         (compare reg opand)
         (jump-if relop label)]
  [trg   reg label]
  [triv  trg int64]
  [opand int64 reg]
  [loc   reg fvar]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [binop * +]
  [relop < <= = >= > !=]
  [int32 int32?]
  [int64 int64?]
  [fvar fvar?]
  [label label?]
]

(define (interp-paren-x64-fvars-v4 x)
  (interp-base `(module ,(append x (list '(halt rax))))))

@define-grammar/pred[paren-x64-rt-v4
  #:literals (int64? aloc? info? any fvar? label? int32?
                     frame-base-pointer-register? dispoffset? natural-number/c)
  #:datum-literals (module true false not begin if set! * + < <= = >= > !=
                           with-label jump compare jump-if
                           rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13
                           r14 r15)
  [p     (begin s ...)]
  [s     (set! addr int32)
         (set! addr trg)
         (set! reg loc)
         (set! reg triv)
         (set! reg_1 (binop reg_1 int32))
         (set! reg_1 (binop reg_1 loc))
         (jump trg)
         (compare reg opand)
         (jump-if relop pc-addr)]
  [trg   reg pc-addr]
  [triv  trg int64]
  [opand int64 reg]
  [loc   reg addr]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [binop * +]
  [relop < <= = >= > !=]
  [int32 int32?]
  [int64 int64?]
  [pc-addr natural-number/c]
  [dispoffset dispoffset?]
]

@define-grammar/pred[paren-x64-v4
  #:literals (int64? aloc? info? any fvar? label? int32? frame-base-pointer-register? dispoffset?)
  #:datum-literals (module true false not begin if set! * + < <= = >= > !=
                           with-label jump compare jump-if
                           rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13
                           r14 r15)
  [p     (begin s ...)]
  [s     (set! addr int32)
         (set! addr trg)
         (set! reg loc)
         (set! reg triv)
         (set! reg_1 (binop reg_1 int32))
         (set! reg_1 (binop reg_1 loc))
         (with-label label s)
         (jump trg)
         (compare reg opand)
         (jump-if relop label)]
  [trg   reg label]
  [triv  trg int64]
  [opand int64 reg]
  [loc   reg addr]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [binop * +]
  [relop < <= = >= > !=]
  [int64 int64?]
  [int32 int32?]
  [dispoffset dispoffset?]
  [label label?]
]

(define (interp-paren-x64-v4 x)
  (interp-base `(module ,(append x '((halt rax))))))

@(module+ test
   (paren-x64-v4?
    '(begin
       (set! rax 5))))
