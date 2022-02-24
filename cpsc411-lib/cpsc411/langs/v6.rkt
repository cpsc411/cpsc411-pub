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
 "v5.rkt")

(provide (all-defined-out))


@define-grammar/pred[values-lang-v6
  #:literals (name? int64?)
  #:datum-literals (define lambda module let call true false not if * + < <= =
   >= > !=)
  [p     (module (define x (lambda (x ...) tail)) ... tail)]
  [pred  (relop triv triv)
         (true)
         (false)
         (not pred)
         (let ([x value] ...) pred)
         (if pred pred pred)]
  [tail  value
         (let ([x value] ...) tail)
         (if pred tail tail)
         (call x triv ...)]
  [value triv
         (binop triv triv)
         (let ([x value] ...) value)
         (if pred value value)
         (call x triv ...)]
  [triv  int64 x]
  [x     name?]
  [binop * + -]
  [relop < <= = >= > !=]
  [int64 int64?]
]

(define (interp-values-lang-v6 x)
  (interp-base x))

@define-grammar/pred[values-unique-lang-v6
  #:literals (name? int64? label? aloc?)
  #:datum-literals (define lambda module let call true false not if * + < <= =
   >= > !=)
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
         (call triv opand ...)]
  [value triv
         (binop opand opand)
         (let ([aloc value] ...) value)
         (if pred value value)
         (call triv opand ...)]
  [opand int64 aloc]
  [triv  opand label]
  [binop * + -]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [label label?]
]

(define (interp-values-unique-lang-v6 x)
  (interp-base x))

@define-grammar/pred[imp-mf-lang-v6
  #:literals (int64? label? aloc? register? fvar? info? any info/c)
  #:datum-literals (new-frames return-point define lambda module begin jump set! halt true false not if
   * + < <= = >= > !=)
  [p      (module (define label (lambda (aloc ...) tail)) ... tail)]
  [pred   (relop opand opand)
          (true)
          (false)
          (not pred)
          (begin effect ... pred)
          (if pred pred pred)]
  [tail   value
          (call triv opand ...)
          (begin effect ... tail)
          (if pred tail tail)]
  [value  triv
          (call triv opand ...)
          (binop opand opand)
          (begin effect ... value)
          (if pred value value)]
  [effect (set! aloc value)
          (begin effect ... effect)
          (if pred effect effect)]
  [opand int64 aloc]
  [triv  opand label]
  [binop  * + -]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
]

(define (interp-imp-mf-lang-v6 x)
  (interp-base x))

@define-grammar/pred[proc-imp-cmf-lang-v6
  #:literals (int64? label? aloc? info?)
  #:datum-literals (define lambda module begin set! halt call true false not if
   * + < <= = >= > !=)
  [p      (module (define label (lambda (aloc ...) entry)) ...
            entry)]
  [entry  tail]
  [pred   (relop opand opand)
          (true)
          (false)
          (not pred)
          (begin effect ... pred)
          (if pred pred pred)]
  [tail   value
          (call triv opand ...)
          (begin effect ... tail)
          (if pred tail tail)]
  [value  triv
          (binop opand opand)
          (call triv opand ...)]
  [effect (set! aloc value)
          (begin effect ... effect)
          (if pred effect effect)]
  [opand int64 aloc]
  [triv  opand label]
  [binop  * + -]
  [relop  < <= = >= > !=]
  [int64 int64?]
  [aloc  aloc?]
  [label  label?]
]

(define (interp-proc-imp-cmf-lang-v6 x)
  (interp-base x))

@define-grammar/pred[imp-cmf-lang-v6
  #:literals (int64? label? aloc? register? fvar? info? info/c)
  #:datum-literals (define lambda module begin halt jump set! true false not if
   * + < <= = >= > != return-point)
  [p      (module info (define label info tail) ... tail)]
  [info   #:with-contract
          (info/c
           (new-frames (frame ...)))
          (let ([frame? (listof aloc?)])
            (info/c
             (new-frames (frame? ...))))]
  [frame  (aloc ...)]
  [pred   (relop opand opand)
          (true)
          (false)
          (not pred)
          (begin effect ... pred)
          (if pred pred pred)]
  [tail   (jump trg loc ...)
          (begin effect ... tail)
          (if pred tail tail)]
  [value  triv
          (binop opand opand)]
  [effect (set! loc value)
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [binop  * + -]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-imp-cmf-lang-v6 x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v6
  #:literals (int64? label? aloc? register? fvar? info? info/c)
  #:datum-literals (define module begin set! jump true false not if * + < <= =
   >= > != halt return-point new-frames)
  [p    (module info (define label info tail) ... tail)]
  [info   #:with-contract
          (info/c
           (new-frames (frame ...)))
          ;info?
          (let ([frame? (listof aloc?)])
            (info/c
             (new-frames (frame? ...))))]
  [frame  (aloc ...)]
  [pred   (relop loc opand)
          (true)
          (false)
          (not pred)
          (begin effect ... pred)
          (if pred pred pred)]
  [tail   #;(halt opand)
          (jump trg loc ...)
          (begin effect ... tail)
          (if pred tail tail)]
  [effect (set! loc triv)
          (set! loc_1 (binop loc_1 opand))
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [binop  * + -]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

@define-grammar/pred[asm-pred-lang-v6/locals
  #:literals (int64? label? aloc? register? fvar? info? info/c)
  #:datum-literals (new-frames locals define module begin set! jump true false not if * + -
                           < <= = >= > !=)
  [p    (module info (define label info tail) ... tail)]
  [info   #:with-contract
          (info/c
           (new-frames (frame ...))
           (locals (aloc ...)))
          ;info?
          (let ([frame? (listof aloc?)])
            (info/c
             (new-frames (frame? ...))
             (locals (aloc? ...))))]
  [frame  (aloc ...)]
  [pred   (relop loc opand)
          (true)
          (false)
          (not pred)
          (begin effect ... pred)
          (if pred pred pred)]
  [tail   #;(halt opand)
          (jump trg loc ...)
          (begin effect ... tail)
          (if pred tail tail)]
  [effect (set! loc triv)
          (set! loc_1 (binop loc_1 opand))
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [binop  * + -]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

@define-grammar/pred[asm-pred-lang-v6/undead
  #:literals (int64? label? aloc? register? fvar? info? undead-set-tree/rloc? undead-set-tree? info/c)
  #:datum-literals (call-undead undead-out locals define module begin set! jump true false not if * + < <= =
   >= > != - new-frames)
  [p    (module info (define label info tail) ... tail)]
  [info   #:with-contract
          (info/c
           (new-frames (frame ...))
           (locals (aloc ...))
           (call-undead (loc ...))
           (undead-out undead-set-tree/rloc?))
          #;info?
          (let ([frame? (listof aloc?)]
                [loc? (or/c register? aloc? fvar?)]
                [rloc (or/c register? fvar?)])
            (info/c
             (new-frames (frame? ...))
             (locals (aloc? ...))
             (call-undead (loc? ...))
             (undead-out undead-set-tree/rloc?)))]
  [frame  (aloc ...)]
  [pred   (relop loc opand)
          (true)
          (false)
          (not pred)
          (begin effect ... pred)
          (if pred pred pred)]
  [tail   #;(halt opand)
          (jump trg loc ...)
          (begin effect ... tail)
          (if pred tail tail)]
  [effect (set! loc triv)
          (set! loc_1 (binop loc_1 opand))
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [binop  * + -]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

@define-grammar/pred[asm-pred-lang-v6/conflicts
  #:literals (int64? label? aloc? register? fvar? info? undead-set-tree? info/c)
  #:datum-literals (conflicts undead-out locals define module begin set! jump
                              true false not if * + - < <= = >= > !=)
  [p    (module info (define label info tail) ... tail)]
  [info   #:with-contract
          (info/c
           (new-frames (frame ...))
           (locals (aloc ...))
           (call-undead (loc ...))
           (undead-out undead-set-tree/rloc?)
           (conflicts ((loc (loc ...)) ...)))
          #;info?
          (let ([frame? (listof aloc?)]
                [loc? (or/c register? aloc? fvar?)]
                [rloc (or/c register? fvar?)])
            (info/c
             (new-frames (frame? ...))
             (locals (aloc? ...))
             (call-undead (loc? ...))
             (undead-out undead-set-tree/rloc?)
             (conflicts ((loc? (loc? ...)) ...))))]
  [frame  (aloc ...)]
  [pred   (relop loc opand)
          (true)
          (false)
          (not pred)
          (begin effect ... pred)
          (if pred pred pred)]
  [tail   #;(halt opand)
          (jump trg loc ...)
          (begin effect ... tail)
          (if pred tail tail)]
  [effect (set! loc triv)
          (set! loc_1 (binop loc_1 opand))
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [binop  * + -]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-asm-pred-lang-v6/conflicts x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v6/pre-framed
  #:literals (int64? label? aloc? register? fvar? info? undead-set-tree/rloc? info/c)
  #:datum-literals (new-frames call-undead assignment conflicts undead-out locals define module begin set! jump
                              true false not if * + - < <= = >= > != halt)
  [p    (module info (define label info tail) ... tail)]
  [info   #:with-contract
          (info/c
           (new-frames (frame ...))
           (locals (aloc ...))
           (call-undead (loc ...))
           (undead-out undead-set-tree/rloc?)
           (conflicts ((loc (loc ...)) ...))
           (assignment ((aloc loc) ...)))
          (let ([frame? (listof aloc?)]
                [loc? (or/c register? aloc? fvar?)]
                [rloc? (or/c register? fvar?)])
            (info/c
             (new-frames (frame? ...))
             (locals (aloc? ...))
             (call-undead (loc? ...))
             (undead-out undead-set-tree/rloc?)
             (conflicts ((loc? (loc? ...)) ...))
             (assignment ((aloc? rloc?) ...))))]
  [frame  (aloc ...)]
  [pred   (relop loc opand)
          (true)
          (false)
          (not pred)
          (begin effect ... pred)
          (if pred pred pred)]
  [tail   #;(halt opand)
          (jump trg loc ...)
          (begin effect ... tail)
          (if pred tail tail)]
  [effect (set! loc triv)
          (set! loc_1 (binop loc_1 opand))
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [binop  * + -]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-asm-pred-lang-v6/pre-framed x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v6/framed
  #:literals (int64? label? aloc? register? fvar? info? undead-set-tree?)
  #:datum-literals (conflicts undead-out locals define module begin set! jump
                              true false not if * + - < <= = >= > !=)
  [p    (module info (define label info tail) ... tail)]
  [info   #:with-contract
          (info/c
           (locals (aloc ...))
           (undead-out undead-set-tree/rloc?)
           (conflicts ((loc (loc ...)) ...))
           (assignment ((aloc loc) ...)))
          (let ([frame? (listof aloc?)]
                [loc? (or/c register? aloc? fvar?)]
                [rloc? (or/c register? fvar?)])
            (info/c
             (locals (aloc? ...))
             (undead-out undead-set-tree/rloc?)
             (conflicts ((loc? (loc? ...)) ...))
             (assignment ((aloc? rloc?) ...))))]
  [pred   (relop loc opand)
          (true)
          (false)
          (not pred)
          (begin effect ... pred)
          (if pred pred pred)]
  [tail   #;(halt opand)
          (jump trg loc ...)
          (begin effect ... tail)
          (if pred tail tail)]
  [effect (set! loc triv)
          (set! loc_1 (binop loc_1 opand))
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [binop  * + -]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-asm-pred-lang-v6/framed x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v6/spilled
  #:literals (int64? label? aloc? register? fvar? info? undead-set-tree? info/c)
  #:datum-literals (assignments conflicts undead-out locals define module begin set! jump
                              true false not if * + - < <= = >= > !=)
  [p    (module info (define label info tail) ... tail)]
  [info   #:with-contract
          (info/c
           (locals (aloc ...))
           (undead-out undead-set-tree/rloc?)
           (conflicts ((loc (loc ...)) ...))
           (assignment ((aloc loc) ...)))
          (let ([loc? (or/c register? aloc? fvar?)]
                [rloc? (or/c register? fvar?)])
            (info/c
             (locals (aloc? ...))
             (undead-out undead-set-tree/rloc?)
             (conflicts ((loc? (loc? ...)) ...))
             (assignment ((aloc? rloc?) ...))))]
  [pred   (relop loc opand)
          (true)
          (false)
          (not pred)
          (begin effect ... pred)
          (if pred pred pred)]
  [tail   #;(halt opand)
          (jump trg loc ...)
          (begin effect ... tail)
          (if pred tail tail)]
  [effect (set! loc triv)
          (set! loc_1 (binop loc_1 opand))
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [binop  * + -]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-asm-pred-lang-v6/spilled x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v6/assignments
  #:literals (int64? label? aloc? register? fvar? info? undead-set-tree? info/c)
  #:datum-literals (assignment conflicts undead-out locals define module begin set! jump
                              true false not if * + - < <= = >= > !=)
  [p    (module info (define label info tail) ... tail)]
  [info   #:with-contract
          (info/c
           (assignment ((aloc loc) ...)))
          (let ([frame? (listof aloc?)]
                [loc? (or/c register? fvar?)])
            (info/c
             (assignment ((aloc? loc?) ...))))]
  [frame  (aloc ...)]
  [pred   (relop loc opand)
          (true)
          (false)
          (not pred)
          (begin effect ... pred)
          (if pred pred pred)]
  [tail   #;(halt opand)
          (jump trg loc ...)
          (begin effect ... tail)
          (if pred tail tail)]
  [effect (set! loc triv)
          (set! loc_1 (binop loc_1 opand))
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [binop  * + -]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-asm-pred-lang-v6/assignments x)
  (interp-base x))

@define-grammar/pred[nested-asm-lang-fvars-v6
  #:literals (int64? register? label? aloc? info? fvar?)
  #:datum-literals (define module begin set! true false not if * + - < <= = >= >
                     != jump rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14
                     r15 halt return-point)
  [p     (module (define label tail) ... tail)]
  [pred  (relop loc opand)
         (true)
         (false)
         (not pred)
         (begin effect ... pred)
         (if pred pred pred)]
  [tail  #;(halt opand)
         (jump trg)
         (begin effect ... tail)
         (if pred tail tail)]
  [effect (set! loc triv)
          (set! loc_1 (binop loc_1 opand))
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [trg   label loc]
  [loc   reg fvar]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * + -]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [fvar fvar?]
  [label label?]
]

(define (interp-nested-asm-lang-fvars-v6 x)
  (interp-base x))

@define-grammar/pred[nested-asm-lang-v6
  #:literals (int64? register? label? aloc? info? fvar?
                     frame-base-pointer-register? dispoffset?)
  #:datum-literals (define module begin set! true false not if * + - < <= = >= >
                     != jump rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14
                     r15 halt return-point)
  [p     (module (define label tail) ... tail)]
  [pred  (relop loc opand)
         (true)
         (false)
         (not pred)
         (begin effect ... pred)
         (if pred pred pred)]
  [tail  #;(halt opand)
         (jump trg)
         (begin effect ... tail)
         (if pred tail tail)]
  [effect (set! loc triv)
          (set! loc_1 (binop loc_1 opand))
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [triv  opand label]
  [opand int64 loc]
  [trg   label loc]
  [loc   reg addr]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * + -]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [dispoffset dispoffset?]
  [label label?]
]

(define (interp-nested-asm-lang-v6 x)
  (interp-base x))

@define-grammar/pred[block-pred-lang-v6
  #:literals (int64? register? label? aloc? info? fvar?
                     frame-base-pointer-register? dispoffset?)
  #:datum-literals (define module begin set! true false not if * + < <= = >= >
   != jump rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15)
  [p     (module b ... b)]
  [b     (define label tail)]
  [pred  (relop loc opand)
         (true)
         (false)
         (not pred)]
  [tail  #;(halt triv)
         (jump trg)
         (begin s ... tail)
         (if pred (jump trg) (jump trg))]
  [s     (set! loc triv)
         (set! loc_1 (binop loc_1 opand))]
  [triv  opand label]
  [opand int64 loc]
  [trg   label loc]
  [loc   reg addr]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * + -]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [dispoffset dispoffset?]
  [label label?]
]

(define (interp-block-pred-lang-v6 x)
  (interp-base x))

@define-grammar/pred[block-asm-lang-v6
  #:literals (int64? aloc? info? any fvar? label? frame-base-pointer-register? dispoffset?)
  #:datum-literals (module true false not begin if set! * + < <= = >= > halt jump)
  [p     (module b ... b)]
  [b     (define label tail)]
  [tail  #;(halt triv)
         (jump trg)
         (begin s ... tail)
         (if (relop loc opand) (jump trg) (jump trg))]
  [s     (set! loc triv)
         (set! loc_1 (binop loc_1 opand))]
  [triv  opand label]
  [opand int64 loc]
  [trg   label loc]
  [loc   reg addr]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * + -]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [dispoffset dispoffset?]
  [label label?]
]

(define (interp-block-asm-lang-v6 x)
  (interp-base x))

@define-grammar/pred[para-asm-lang-v6
#:literals (int64? aloc? info? any fvar? label? frame-base-pointer-register? dispoffset?)
#:datum-literals (halt true false not begin if set! * + < <= = >= > !=
                       with-label jump compare jump-if rsp rbp rax rbx rcx rdx
                       rsi rdi r8 r9 r12 r13 r14 r15)
[p     (begin s ...)]
[s     #;(halt opand)
       (set! loc triv)
       (set! loc_1 (binop loc_1 opand))
       (jump trg)
       (with-label label s)
       (compare loc opand)
       (jump-if relop trg)]
[triv  opand label]
[opand int64 loc]
[trg   label loc]
[loc   reg addr]
[reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
[binop + * -]
[relop < <= = >= > !=]
[int64 int64?]
[aloc aloc?]
[addr  (fbp - dispoffset)]
[fbp   frame-base-pointer-register?]
[dispoffset dispoffset?]
[label label?]
]

(define (interp-para-asm-lang-v6 x)
  (interp-para-asm-lang-v5 x))

@define-grammar/pred[paren-x64-rt-v6
  #:literals (int64? aloc? info? any fvar? label? int32? frame-base-pointer-register? dispoffset?)
  #:datum-literals (module true false not begin if set! * + < <= = >= > with-label jump compare jump-if)
  [p     (begin s ...)]
  [s     (set! loc triv)
         (set! reg fvar)
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
  [binop * + -]
  [relop < <= = >= > !=]
  [int32 int32?]
  [int64 int64?]
  [label label?]
  [dispoffset dispoffset?]
]

@define-grammar/pred[paren-x64-v6
  #:literals (int64? aloc? info? any fvar? label? int32? frame-base-pointer-register? dispoffset?)
  #:datum-literals (module true false not begin if set! * + < <= = >= > with-label jump compare jump-if)
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
         (jump-if relop label?)]
  [trg   reg label]
  [triv  trg int64]
  [opand int64 reg]
  [loc   reg addr]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [binop * + -]
  [relop < <= = >= > !=]
  [int32 int32?]
  [int64 int64?]
  [label label?]
  [dispoffset dispoffset?]
]

(define (interp-paren-x64-v6 x)
  (interp-paren-x64-v5 x))
