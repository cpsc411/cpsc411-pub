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
 "v6.rkt")

(provide (all-defined-out))

@define-grammar/pred[exprs-lang-v7
#:literals (name? int61? uint8? ascii-char-literal?)
#:datum-literals (module lambda define let if void error * + - eq? < <= >
                         >= fixnum? boolean? empty? void? ascii-char? error? not
                         call empty)
[p     (module b ... e)]
[b     (define x (lambda (x ...) e))]
[e     v
       (call e e ...)
       (let ([x e] ...) e)
       (if e e e)]
[x     name? prim-f]
[v     x fixnum #t #f empty (void) (error uint8) ascii-char-literal]
[prim-f binop unop]
[binop  * + - eq? < <= > >=]
[unop   fixnum? boolean? empty? void? ascii-char? error? not]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]


(module safe-langs racket/base
  (require
   "base.rkt"
   cpsc411/machine-ints
   (only-in racket/base
            [module+ r:module+] [define r:define]))

  (provide
   (all-from-out "base.rkt")
   (all-defined-out))

  (r:define (wrap-error-ret who unsafe)
    (lambda ops
      (with-handlers ([exn:fail:contract? (λ (e) (error who))])
        (apply unsafe ops))))

  (define-syntax-rule (define-error-ret f unsafe)
    (r:define f (wrap-error-ret 'f unsafe)))

  (define-error-ret + unsafe-fx+)
  (define-error-ret - unsafe-fx-)
  (define-error-ret * unsafe-fx*)
  (define-error-ret < unsafe-fx<)
  (define-error-ret > unsafe-fx>)
  (define-error-ret <= unsafe-fx<=)
  (define-error-ret >= unsafe-fx>=)

  (r:module+ interp
             (provide interp-exprs-lang-v7)
             (define-namespace-anchor a)
             (r:define interp-exprs-lang-v7
                       (let ([ns (namespace-anchor->namespace a)])
                         (lambda (x)
                           (eval x ns))))))

(require (submod 'safe-langs interp))
(provide interp-exprs-lang-v7)

@define-grammar/pred[exprs-unique-lang-v7
#:literals (aloc? label? int64? int61? uint8? ascii-char-literal?)
#:datum-literals (module lambda define apply let if void error * + - eq? < <= >
                         >= fixnum? boolean? empty? void? ascii-char? error? not
                         call)
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[e     v
       (call e e ...)
       (let ([aloc e] ...) e)
       (if e e e)]
[v     label aloc prim-f fixnum #t #f empty (void) (error uint8) ascii-char-literal]
[prim-f binop unop]
[binop  * + - < eq? <= > >=]
[unop   fixnum? boolean? empty? void? ascii-char? error? not]
[aloc aloc?]
[label label?]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]

@define-grammar/pred[exprs-unsafe-data-lang-v7
#:literals (aloc? label? int64? int61? uint8? ascii-char-literal?)
#:datum-literals (module lambda define apply let if begin void error
                         >= fixnum? boolean? empty? void? ascii-char? error? not
                         call
                         unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx<
                         unsafe-fx<= unsafe-fx> unsafe-fx>= true false)
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
       (if pred e e)]
[v     label aloc fixnum #t #f empty (void) (error uint8) ascii-char-literal]
[primop binop unop]
[binop  unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
        unsafe-fx>=]
[unop   fixnum? boolean? empty? void? ascii-char? error? not]
[aloc aloc?]
[label label?]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]

@define-grammar/pred[exprs-bits-lang-v7
#:literals (aloc? label? int64?)
#:datum-literals (module lambda define apply let if void error * + - = != < <= >
                         >=  bitwise-and bitwise-ior bitwise-xor
                         arithmetic-shift-right true false not call)
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[pred  (relop e e)
       (true)
       (false)
       (not pred)
       (let ([aloc e] ...) pred)
       (if pred pred pred)]
[e     v
       (binop e e)
       (call e e ...)
       (let ([aloc e] ...) e)
       (if pred e e)]
[v     label aloc int64]
[binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
[aloc aloc?]
[label label?]
[relop < <= = >= > !=]
[int64 int64?]
]

@define-grammar/pred[exprs-bits-lang-v7/contexts
#:literals (aloc? label? int64?)
#:datum-literals (module lambda define apply let if void error * + - = != < <= >
                         >=  bitwise-and bitwise-ior bitwise-xor
                         arithmetic-shift-right true false not call)
[p     (module b ... tail)]
[b     (define label (lambda (aloc ...) tail))]
[pred  (relop value value)
       (true)
       (false)
       (not pred)
       (let ([aloc value] ...) pred)
       (if pred pred pred)]
[tail  value
       (let ([aloc value] ...) tail)
       (if pred tail tail)]
[value triv
       (binop value value)
       (call value value ...)
       (let ([aloc value] ...) value)
       (if pred value value)]
[triv label aloc int64]
[binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
[aloc aloc?]
[label label?]
[relop < <= = >= > !=]
[int64 int64?]
]

@define-grammar/pred[values-bits-lang-v7
  #:literals (label? aloc? int64?)
  #:datum-literals (define lambda module let call true false not if * + < <= =
                     >= > != bitwise-and bitwise-ior bitwise-xor
                     arithmetic-shift-right)
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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [label label?]
]

@define-grammar/pred[proc-imp-mf-lang-v7
  #:literals (int64? label? aloc? info?)
  #:datum-literals (define lambda module begin set! halt call true false not if
                     * + < <= = >= > != bitwise-and bitwise-ior bitwise-xor
                     arithmetic-shift-right)
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
          (begin effect ... value)
          (if pred value value)
          (call triv opand ...)]
  [effect (set! aloc value)
          (begin effect ... effect)
          (if pred effect effect)]
  [opand int64 aloc]
  [triv  opand label]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64 int64?]
  [aloc  aloc?]
  [label  label?]
]

@define-grammar/pred[imp-mf-lang-v7
  #:literals (int64? label? aloc? register? fvar? info? any info/c)
  #:datum-literals (new-frames return-point define lambda module begin jump set! halt true false not if
                               * + < <= = >= > != bitwise-and bitwise-ior
                               bitwise-xor arithmetic-shift-right)
  [p      (module info (define label info tail) ... tail)]

  [info #:with-contract
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
  [tail   ;value
          (jump trg loc ...)
          (begin effect ... tail)
          (if pred tail tail)]
  [value  triv
          (binop opand opand)
          (begin effect ... value)
          (if pred value value)
          (return-point label tail)]
  [effect (set! loc value)
          (begin effect ... effect)
          (if pred effect effect)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]


@define-grammar/pred[imp-cmf-lang-v7
  #:literals (int64? label? aloc? register? fvar? info? info/c)
  #:datum-literals (define lambda module begin halt jump set! true false not if
                     * + < <= = >= > != return-point bitwise-and bitwise-ior
                     bitwise-xor arithmetic-shift-right)
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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

@define-grammar/pred[asm-pred-lang-v7
  #:literals (int64? label? aloc? register? fvar? info? info/c)
  #:datum-literals (define module begin set! jump true false not if * + < <= =
                     >= > != halt return-point new-frames bitwise-and
                     bitwise-ior bitwise-xor arithmetic-shift-right)
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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

@define-grammar/pred[asm-pred-lang-v7/locals
  #:literals (int64? label? aloc? register? fvar? info? info/c)
  #:datum-literals (new-frames locals define module begin set! jump true false not if * + -
                               < <= = >= > != bitwise-and bitwise-ior
                               bitwise-xor arithmetic-shift-right)
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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

@define-grammar/pred[asm-pred-lang-v7/undead
  #:literals (int64? label? aloc? register? fvar? info? undead-set-tree/rloc? undead-set-tree? info/c)
  #:datum-literals (call-undead undead-out locals define module begin set! jump true false not if * + < <= =
                                >= > != - new-framesbitwise-and bitwise-ior
                                bitwise-xor arithmetic-shift-right)
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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

@define-grammar/pred[asm-pred-lang-v7/conflicts
  #:literals (int64? label? aloc? register? fvar? info? undead-set-tree? info/c)
  #:datum-literals (conflicts undead-out locals define module begin set! jump
                              true false not if * + - < <= = >= > != bitwise-and
                              bitwise-ior bitwise-xor arithmetic-shift-right)
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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

@define-grammar/pred[asm-pred-lang-v7/pre-framed
  #:literals (int64? label? aloc? register? fvar? info? undead-set-tree/rloc? info/c)
  #:datum-literals (new-frames call-undead assignment conflicts undead-out locals define module begin set! jump
                               true false not if * + - < <= = >= > != halt
                               bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right)
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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

@define-grammar/pred[asm-pred-lang-v7/framed
  #:literals (int64? label? aloc? register? fvar? info? undead-set-tree?)
  #:datum-literals (conflicts undead-out locals define module begin set! jump
                              true false not if * + - < <= = >= > != bitwise-and
                              bitwise-ior bitwise-xor arithmetic-shift-right)
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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

@define-grammar/pred[asm-pred-lang-v7/spilled
  #:literals (int64? label? aloc? register? fvar? info? undead-set-tree? info/c)
  #:datum-literals (assignments conflicts undead-out locals define module begin set! jump
                                true false not if * + - < <= = >= > !=
                                bitwise-and bitwise-ior bitwise-xor
                                arithmetic-shift-right)
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
  [binop  * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

@define-grammar/pred[asm-pred-lang-v7/assignments
  #:literals (int64? label? aloc? register? fvar? info? undead-set-tree? info/c)
  #:datum-literals (assignment conflicts undead-out locals define module begin set! jump
                               true false not if * + - < <= = >= > !=
                               bitwise-and bitwise-ior bitwise-xor
                               arithmetic-shift-right)
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
  [binop  * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

@define-grammar/pred[nested-asm-lang-fvars-v7
  #:literals (int64? register? label? aloc? info? fvar?)
  #:datum-literals (define module begin set! true false not if * + - < <= = >= >
                     != jump rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14
                     r15 halt bitwise-and bitwise-ior bitwise-xor
                     arithmetic-shift-right)
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
  [loc   reg fvar]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [fvar fvar?]
  [label label?]
]

@define-grammar/pred[nested-asm-lang-v7
  #:literals (int64? register? label? aloc? info? fvar?
                     frame-base-pointer-register? dispoffset?)
  #:datum-literals (define module begin set! true false not if * + - < <= = >= >
                     != jump rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14
                     r15 halt bitwise-and bitwise-ior bitwise-xor
                     arithmetic-shift-right)
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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [dispoffset dispoffset?]
  [label label?]
]

@define-grammar/pred[block-pred-lang-v7
  #:literals (int64? register? label? aloc? info? fvar?
                     frame-base-pointer-register? dispoffset?)
  #:datum-literals (define module begin set! true false not if * + < <= = >= >
                     != jump rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14
                     r15 bitwise-and bitwise-ior bitwise-xor
                     arithmetic-shift-right)
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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [dispoffset dispoffset?]
  [label label?]
]

@define-grammar/pred[block-asm-lang-v7
  #:literals (int64? aloc? info? any fvar? label? frame-base-pointer-register? dispoffset?)
  #:datum-literals (module true false not begin if set! * + < <= = >= > halt
                           jump bitwise-and bitwise-ior bitwise-xor
                           arithmetic-shift-right)
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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [dispoffset dispoffset?]
  [label label?]
]


@define-grammar/pred[para-asm-lang-v7
#:literals (int64? aloc? info? any fvar? label? frame-base-pointer-register? dispoffset?)
#:datum-literals (halt true false not begin if set! * + < <= = >= > !=
                       with-label jump compare jump-if rsp rbp rax rbx rcx rdx
                       rsi rdi r8 r9 r12 r13 r14 r15 bitwise-and bitwise-ior
                       bitwise-xor arithmetic-shift-right)
[p     (begin s ...)]
[s     #;(halt opand)
       (set! loc triv)
       (set! loc_1 (binop loc_1 opand))
       (with-label label s)
       (jump trg)
       (compare loc opand)
       (jump-if relop trg)]
[trg   label loc]
[triv  opand label]
[opand int64 loc]
[loc   reg addr]
[reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
[addr  (fbp - dispoffset)]
[fbp   frame-base-pointer-register?]
[binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
[relop < <= = >= > !=]
[int64 int64?]
[label label?]
[dispoffset dispoffset?]
]

@define-grammar/pred[paren-x64-rt-v7
  #:literals (int64? aloc? info? any fvar? label? int32? frame-base-pointer-register? dispoffset?)
  #:datum-literals (module true false not begin if set! * + < <= = >= >
                           with-label jump compare jump-if bitwise-and
                           bitwise-ior bitwise-xor arithmetic-shift-right)
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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int32 int32?]
  [int64 int64?]
  [label label?]
  [dispoffset dispoffset?]
]

@define-grammar/pred[paren-x64-v7
  #:literals (int64? aloc? info? any fvar? label? int32? frame-base-pointer-register? dispoffset?)
  #:datum-literals (module true false not begin if set! * + < <= = >= >
                           with-label jump compare jump-if bitwise-and
                           bitwise-ior bitwise-xor arithmetic-shift-right)
  [p     (begin s ...)]
  [s     (set! addr int32)
         (set! addr trg)
         (set! reg loc)
         (set! reg triv)
         (set! reg_1 (binop reg_1 int32))
         (set! reg_1 (binop reg_1 loc))
         (with-label label? s)
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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int32 int32?]
  [int64 int64?]
  [label label?]
  [dispoffset dispoffset?]
]
