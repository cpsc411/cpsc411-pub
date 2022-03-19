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
 "v7.rkt")

(provide (all-defined-out))

@define-grammar/pred[exprs-lang-v8
#:literals (name? int61? uint8? ascii-char-literal?)
#:datum-literals (module lambda define apply let if void error * + - eq? < <= >
                         >= fixnum? boolean? empty? void? ascii-char? error? not
                         call make-vector vector-length vector-set! vector-ref
                         car cons cdr vector? pair?)
[p     (module (define x (lambda (x ...) value)) ... value)]
[value triv
       (let ([x value] ...) value)
       (if value value value)
       (call value value ...)]
[triv  x fixnum #t #f empty (void) (error uint8) ascii-char-literal]
[x     name? prim-f]
[prim-f * + - < <= > >= eq?
        fixnum? boolean? empty? void? ascii-char? error? not
        pair? vector? cons car cdr make-vector vector-length vector-set! vector-ref]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
[fixnum int61?]
]

(define (interp-exprs-lang-v8 x)
  (interp-exprs-lang-v7 x))

@define-grammar/pred[exprs-unique-lang-v8
#:literals (aloc? label? int64? int61? uint8? ascii-char-literal?)
#:datum-literals (module lambda define apply let if void error * + - eq? < <= >
                         >= fixnum? boolean? empty? void? ascii-char? error? not
                         call

                         pair? vector? cons car cdr make-vector vector-length vector-set! vector-ref)
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[e     v
       (call e e ...)
       (let ([aloc e] ...) e)
       (if e e e)]
[v     label aloc prim-f fixnum #t #f empty (void) (error uint8) ascii-char-literal]
[prim-f * + - < <= > >= eq?
        fixnum? boolean? empty? void? ascii-char? error? not
        pair? vector? cons car cdr make-vector vector-length vector-set! vector-ref]
[aloc aloc?]
[label label?]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]

(define (interp-exprs-unique-lang-v8 x)
  (interp-exprs-lang-v7 x))

@define-grammar/pred[exprs-unsafe-data-lang-v8
#:literals (aloc? label? int64? int61? uint8? ascii-char-literal?)
#:datum-literals (module lambda define call let if begin void error
                         >= fixnum? boolean? empty? void? ascii-char? error?
                         unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
                         unsafe-fx>=

                         not pair? vector?

                         cons unsafe-car unsafe-cdr

                         unsafe-make-vector
                         unsafe-vector-length
                         unsafe-vector-set!
                         unsafe-vector-ref)
[p     (module (define label (lambda (aloc ...) value)) ... value)]
[pred  value
       (true)
       (false)
       (not pred)
       (let ([aloc value] ...) pred)
       (if pred pred pred)]
[value triv
       (primop value ...)
       (call value value ...)
       (let ([aloc value] ...) value)
       (if pred value value)
       (begin effect ... value)]
[effect (primop value ...) (begin effect ... effect)]
[triv   label aloc fixnum #t #f empty (void) (error uint8) ascii-char-literal]
[primop  unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
         unsafe-fx>=

         fixnum? boolean? empty? void? ascii-char? error? not
         pair? vector?

         cons unsafe-car unsafe-cdr

         unsafe-make-vector
         unsafe-vector-length
         unsafe-vector-set!
         unsafe-vector-ref]
[aloc aloc?]
[label label?]
[fixnum int61?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
]

(define (interp-exprs-unsafe-data-lang-v8 x)
  (interp-base x))

@define-grammar/pred[exprs-bits-lang-v8
#:literals (aloc? label? int64?)
#:datum-literals (module lambda define apply let if void error * + - = != < <= >
                         >=  bitwise-and bitwise-ior bitwise-xor
                         arithmetic-shift-right true false not call
                         mref mset! begin alloc)
[p     (module (define label (lambda (aloc ...) value)) ... value)]
[pred  (relop value value)
       (true)
       (false)
       (not pred)
       (let ([aloc value] ...) pred)
       (if pred pred pred)
       (begin effect ... pred)]
[value triv
       (binop value value)
       (mref value value)
       (alloc value)
       (call value value ...)
       (let ([aloc value] ...) value)
       (if pred value value)
       (begin effect ... value)]
[effect (mset! value value value)
        (begin effect ... effect)]
[triv label aloc int64]
[binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
[aloc aloc?]
[label label?]
[relop < <= = >= > !=]
[int64 int64?]
]

(define (interp-exprs-bits-lang-v8 x)
  (interp-base x))

@define-grammar/pred[exprs-bits-lang-v8/contexts
#:literals (aloc? label? int64?)
#:datum-literals (module lambda define apply let if void error * + - = != < <= >
                         >=  bitwise-and bitwise-ior bitwise-xor
                         arithmetic-shift-right true false not call
                         mref mset! begin alloc)
[p     (module (define label (lambda (aloc ...) tail)) ... tail)]
[pred  (relop value value)
       (true)
       (false)
       (not pred)
       (let ([aloc value] ...) pred)
       (if pred pred pred)
       (begin effect ... pred)]
[tail  value
       (let ([aloc value] ...) tail)
       (if pred tail tail)
       (begin effect ... tail)]
[value triv
       (binop value value)
       (mref value value)
       (alloc value)
       (call value value ...)
       (let ([aloc value] ...) value)
       (if pred value value)
       (begin effect ... value)]
[effect (mset! value value value)
        (begin effect ... effect)]
[triv label aloc int64]
[binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
[aloc aloc?]
[label label?]
[relop < <= = >= > !=]
[int64 int64?]
]

(define (interp-exprs-bits-lang-v8/contexts x)
  (interp-base x))

@define-grammar/pred[values-bits-lang-v8
  #:literals (label? aloc? int64?)
  #:datum-literals (define lambda module let call true false not if * + < <= =
                     >= > != bitwise-and bitwise-ior bitwise-xor
                     arithmetic-shift-right
                     mref mset! alloc)
  [p     (module (define label (lambda (aloc ...) tail)) ... tail)]
  [pred  (relop opand opand)
         (true)
         (false)
         (not pred)
         (let ([aloc value] ...) pred)
         (if pred pred pred)
         (begin effect ... pred)]
  [tail  value
         (let ([aloc value] ...) tail)
         (if pred tail tail)
         (call triv opand ...)
         (begin effect ... tail)]
  [value triv
         (binop opand opand)
         (mref aloc opand)
         (alloc opand)
         (let ([aloc value] ...) value)
         (if pred value value)
         (call triv opand ...)
         (begin effect ... value)]
  [effect (mset! aloc opand value)
          (let ([aloc value] ...) effect)
          (begin effect ... effect)]
  [opand int64 aloc]
  [triv  opand label]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int64 int64?]
  [aloc aloc?]
  [label label?]
]

(define (interp-values-bits-lang-v8 x)
  (interp-base x))

@define-grammar/pred[imp-mf-lang-v8
  #:literals (int64? label? aloc? register? fvar? info? any info/c)
  #:datum-literals (new-frames return-point define lambda module begin jump set! halt true false not if
                               * + < <= = >= > != bitwise-and bitwise-ior
                               bitwise-xor arithmetic-shift-right
                               mref mset! alloc)
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
          (mref loc opand)
          (alloc opand)
          (begin effect ... value)
          (if pred value value)]
  [effect (set! loc value)
          (mset! loc opand value)
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

(define (interp-imp-mf-lang-v8 x)
  (interp-base x))

@define-grammar/pred[proc-imp-cmf-lang-v8
  #:literals (int64? label? aloc? info?)
  #:datum-literals (define lambda module begin set! halt call true false not if
                     * + < <= = >= > != bitwise-and bitwise-ior bitwise-xor
                     arithmetic-shift-right
                     mref mset! alloc)
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
          (mref aloc opand)
          (alloc opand)
          (call triv opand ...)]
  [effect (set! aloc value)
          (mset! aloc opand value)
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

(define (interp-proc-imp-cmf-lang-v8 x)
  (interp-base x))

@define-grammar/pred[imp-cmf-lang-v8
  #:literals (int64? label? aloc? register? fvar? info? info/c)
  #:datum-literals (define lambda module begin halt jump set! true false not if
                     * + < <= = >= > != return-point bitwise-and bitwise-ior
                     bitwise-xor arithmetic-shift-right
                     mset! mref alloc)
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
          (binop opand opand)
          (mref loc opand)
          (alloc opand)]
  [effect (set! loc value)
          (mset! loc opand triv)
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

(define (interp-imp-cmf-lang-v8 x)
  (interp-base x))

@define-grammar/pred[asm-alloc-lang-v8
  #:literals (int32? int64? label? aloc? register? fvar? info? info/c)
  #:datum-literals (define module begin set! jump true false not if * + < <= =
                     >= > != halt return-point new-frames bitwise-and
                     bitwise-ior bitwise-xor arithmetic-shift-right alloc
                     mref mset!)
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
          (set! loc_1 (mref loc_2 index))
          (set! loc (alloc index))
          (mset! loc index triv)
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [index int64 loc]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [int32  int32?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-asm-alloc-lang-v8 x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v8
  #:literals (int32? int64? label? aloc? register? fvar? info? info/c)
  #:datum-literals (define module begin set! jump true false not if * + < <= =
                     >= > != halt return-point new-frames bitwise-and
                     bitwise-ior bitwise-xor arithmetic-shift-right alloc
                     mref mset!)
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
          (set! loc_1 (mref loc_2 index))
          (mset! loc index triv)
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [index  int64 loc]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [int32  int32?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-asm-pred-lang-v8 x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v8/locals
  #:literals (int32? int64? label? aloc? register? fvar? info? info/c)
  #:datum-literals (new-frames locals define module begin set! jump true false not if * + -
                               < <= = >= > != bitwise-and bitwise-ior
                               bitwise-xor arithmetic-shift-right
                               mref mset!)
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
          (set! loc_1 (mref loc_2 index))
          (begin effect ... effect)
          (mset! loc index triv)
          (if pred effect effect)
          (return-point label tail)]
  [opand  int64 loc]
  [triv   opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [index  int64 loc]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [int32  int32?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-asm-pred-lang-v8/locals x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v8/undead
  #:literals (int32? int64? label? aloc? register? fvar? info? undead-set-tree/rloc? undead-set-tree? info/c)
  #:datum-literals (call-undead undead-out locals define module begin set! jump true false not if * + < <= =
                                >= > != - new-framesbitwise-and bitwise-ior
                                bitwise-xor arithmetic-shift-right
                                mref mset!)
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
          (set! loc_1 (mref loc_2 index))
          (mset! loc index triv)
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand  int64 loc]
  [triv   opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [index  int64 loc]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [int32  int32?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-asm-pred-lang-v8/undead x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v8/conflicts
  #:literals (int32? int64? label? aloc? register? fvar? info? undead-set-tree? info/c)
  #:datum-literals (conflicts undead-out locals define module begin set! jump
                              true false not if * + - < <= = >= > != bitwise-and
                              bitwise-ior bitwise-xor arithmetic-shift-right
                              mref mset!)
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
          (set! loc_1 (mref loc_2 index))
          (mset! loc index triv)
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [index  int64 loc]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [int32  int32?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-asm-pred-lang-v8/conflicts x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v8/pre-framed
  #:literals (int32? int64? label? aloc? register? fvar? info? undead-set-tree/rloc? info/c)
  #:datum-literals (new-frames call-undead assignment conflicts undead-out locals define module begin set! jump
                               true false not if * + - < <= = >= > != halt
                               bitwise-and bitwise-ior bitwise-xor
                               arithmetic-shift-right
                               mref mset!)
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
          (set! loc_1 (mref loc_2 index))
          (mset! loc index triv)
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [index  int64 loc]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [int32  int32?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-asm-pred-lang-v8/pre-framed x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v8/framed
  #:literals (int32? int64? label? aloc? register? fvar? info? undead-set-tree?)
  #:datum-literals (conflicts undead-out locals define module begin set! jump
                              true false not if * + - < <= = >= > != bitwise-and
                              bitwise-ior bitwise-xor arithmetic-shift-right
                              mref mset!)
  [p    (module info (define label info tail) ... tail)]
  [info   #:with-contract
          (info/c
           (locals (aloc ...))
           (conflicts ((loc (loc ...)) ...))
           (assignment ((aloc loc) ...)))
          (let ([loc? (or/c register? aloc? fvar?)]
                [rloc? (or/c register? fvar?)])
            (info/c
             (locals (aloc? ...))
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
          (set! loc_1 (mref loc_2 index))
          (mset! loc index triv)
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [index  int64 loc]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [int32  int32?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-asm-pred-lang-v8/framed x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v8/spilled
  #:literals (int32? int64? label? aloc? register? fvar? info? undead-set-tree? info/c)
  #:datum-literals (assignments conflicts undead-out locals define module begin set! jump
                                true false not if * + - < <= = >= > !=
                                bitwise-and bitwise-ior bitwise-xor
                                arithmetic-shift-right mref mset!)
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
          (set! loc_1 (mref loc_2 index))
          (mset! loc index triv)
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [index  int64 loc]
  [binop  * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [int32  int32?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-asm-pred-lang-v8/spilled x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v8/assignments
  #:literals (int32? int64? label? aloc? register? fvar? info? undead-set-tree? info/c)
  #:datum-literals (assignment conflicts undead-out locals define module begin set! jump
                               true false not if * + - < <= = >= > !=
                               bitwise-and bitwise-ior bitwise-xor
                               arithmetic-shift-right
                               mref mset!)
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
          (set! loc_1 (mref loc_2 index))
          (mset! loc index triv)
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc    rloc aloc]
  [trg    label loc]
  [index  int64 loc]
  [binop  * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [int64  int64?]
  [int32  int32?]
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
]

(define (interp-asm-pred-lang-v8/assignments x)
  (interp-base x))

@define-grammar/pred[nested-asm-lang-fvars-v8
  #:literals (int32? int64? register? label? aloc? info? fvar?)
  #:datum-literals (define module begin set! true false not if * + - < <= = >= >
                     != jump rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14
                     r15 halt bitwise-and bitwise-ior bitwise-xor
                     arithmetic-shift-right
                     mref mset!)
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
          (set! loc_1 (mref loc_2 index))
          (mset! loc index triv)
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [triv  opand label]
  [opand int64 loc]
  [trg   label loc]
  [loc   reg fvar]
  [index  int64 loc]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int64 int64?]
  [int32 int32?]
  [aloc aloc?]
  [fvar fvar?]
  [label label?]
]

(define (interp-nested-asm-lang-fvars-v8 x)
  (interp-base x))

@define-grammar/pred[nested-asm-lang-v8
  #:literals (int32? int64? register? label? aloc? info? fvar?
                     frame-base-pointer-register? dispoffset?)
  #:datum-literals (define module begin set! true false not if * + - < <= = >= >
                     != jump rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14
                     r15 halt bitwise-and bitwise-ior bitwise-xor
                     arithmetic-shift-right
                     mref mset!)
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
          (set! loc_1 (mref loc_2 index))
          (mset! loc index triv)
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [triv  opand label]
  [opand int64 loc]
  [trg   label loc]
  [loc   reg addr]
  [index  int64 loc]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int64 int64?]
  [int32 int32?]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [dispoffset dispoffset?]
  [label label?]
]

(define (interp-nested-asm-lang-v8 x)
  (interp-base x))

@define-grammar/pred[block-pred-lang-v8
  #:literals (int32? int64? register? label? aloc? info? fvar?
                     frame-base-pointer-register? dispoffset?)
  #:datum-literals (define module begin set! true false not if * + < <= = >= >
                     != jump rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14
                     r15 bitwise-and bitwise-ior bitwise-xor
                     arithmetic-shift-right
                     mref mset!)
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
         (set! loc_1 (binop loc_1 opand))
         (set! loc_1 (mref loc_2 index))
         (mset! loc index triv)]
  [triv  opand label]
  [opand int64 loc]
  [trg   label loc]
  [loc   reg addr]
  [index  int64 loc]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int64 int64?]
  [int32 int32?]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [dispoffset dispoffset?]
  [label label?]
]

(define (interp-block-pred-lang-v8 x)
  (interp-base x))

@define-grammar/pred[block-asm-lang-v8
  #:literals (int32? int64? aloc? info? any fvar? label? frame-base-pointer-register? dispoffset?)
  #:datum-literals (module true false not begin if set! * + < <= = >= > halt
                           jump bitwise-and bitwise-ior bitwise-xor
                           arithmetic-shift-right
                           mref mset!)
  [p     (module b ... b)]
  [b     (define label tail)]
  [tail  #;(halt triv)
         (jump trg)
         (begin s ... tail)
         (if (relop loc opand) (jump trg) (jump trg))]
  [s     (set! loc triv)
         (set! loc_1 (binop loc_1 opand))
         (set! loc_1 (mref loc_2 index))
         (mset! loc index triv)]
  [triv  opand label]
  [opand int64 loc]
  [trg   label loc]
  [loc   reg addr]
  [index  int64 loc]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int64 int64?]
  [int32 int32?]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [dispoffset dispoffset?]
  [label label?]
]

(define (interp-block-asm-lang-v8 x)
  (interp-base x))

@define-grammar/pred[para-asm-lang-v8
  #:literals (int32? int64? int32? aloc? info? any fvar? label? frame-base-pointer-register? dispoffset?)
  #:datum-literals (halt true false not begin if set! * + < <= = >= > !=
                         with-label jump compare jump-if rsp rbp rax rbx rcx rdx
                         rsi rdi r8 r9 r12 r13 r14 r15 bitwise-and bitwise-ior
                         bitwise-xor arithmetic-shift-right
                         mref mset!)
  [p     (begin s ...)]
  [s     (set! loc triv)
         (set! loc_1 (binop loc_1 opand))
         (set! loc_1 (mref loc_2 index))
         (mset! loc_1 index triv)
         (with-label label s)
         (jump trg)
         (compare loc opand)
         (jump-if relop trg)]
  [trg   label loc]
  [triv  opand label]
  [opand int64 loc]
  [loc   reg addr]
  [index  int64 loc]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r12 r13 r14 r15]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int64 int64?]
  [int32 int32?]
  [label label?]
  [dispoffset dispoffset?]
]

(define (interp-para-asm-lang-v8 x)
  (interp-para-asm-lang-v7 x))

@define-grammar/pred[paren-x64-mops-v8
  #:literals (int32? int64? aloc? info? any fvar? label? int32? frame-base-pointer-register? dispoffset?)
  #:datum-literals (module true false not begin if set! * + < <= = >= >
                           with-label jump compare jump-if bitwise-and
                           bitwise-ior bitwise-xor arithmetic-shift-right mset!
                           mref)
  [p     (begin s ...)]
  [s     (set! addr int32)
         (set! addr trg)
         (set! reg loc)
         (set! reg triv)
         (set! reg_1 (binop reg_1 int32))
         (set! reg_1 (binop reg_1 loc))
         (set! reg_1 (mref reg_2 index))
         (mset! reg_1 index triv)
         (with-label label s)
         (jump trg)
         (compare reg opand)
         (jump-if relop label)]
  [trg   reg label]
  [triv  trg int64]
  [opand int64 reg]
  [loc   reg addr]
  [index int32 reg]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int64 int64?]
  [int32 int32?]
  [label label?]
  [dispoffset dispoffset?]
]

(define (interp-paren-x64-mops-v8 x)
  (interp-para-asm-lang-v8 x))

@define-grammar/pred[paren-x64-v8
  #:literals (int64? aloc? info? any fvar? label? int32? frame-base-pointer-register? dispoffset?)
  #:datum-literals (module true false not begin if set! * + < <= = != >= >
                           with-label jump compare jump-if bitwise-and
                           bitwise-ior bitwise-xor arithmetic-shift-right
                           rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)
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
  [addr  (fbp - dispoffset) (reg + int32) (reg + reg)]
  [fbp   frame-base-pointer-register?]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int64 int64?]
  [int32 int32?]
  [dispoffset dispoffset?]
  [label label?]
]

(define (interp-paren-x64-v8 x)
  (interp-paren-x64-v7 x))
