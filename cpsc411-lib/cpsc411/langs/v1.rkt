#lang at-exp racket/base

(require
 cpsc411/compiler-lib
 cpsc411/info-lib
 scribble/bettergrammar
 racket/contract
 (for-label cpsc411/compiler-lib)
 (for-label cpsc411/info-lib)
 (for-label racket/contract)
 "../utils/redex-gen.rkt")

(provide (all-defined-out))

@define-grammar/pred[paren-x64-v1
  #:literals (int64? int32?)
  #:datum-literals (begin set! * + rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)
  [p     (begin s ...)]
  [s     (set! reg int64)
         (set! reg reg)
         (set! reg_1 (binop reg_1 int32))
         (set! reg_1 (binop reg_1 reg))]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [binop * +]
  [int64 int64?]
  [int32 int32?]
]
