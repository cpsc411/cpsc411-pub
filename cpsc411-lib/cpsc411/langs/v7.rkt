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
 "v6.rkt")

(provide (all-defined-out))

@define-grammar/pred[exprs-lang-v7
#:literals (name? int61? uint8? ascii-char-literal?)
#:datum-literals (module lambda define let if void error * + - eq? < <= >
                 >= fixnum? boolean? empty? void? ascii-char? error? not
                 call empty)
[p     (module (define x (lambda (x ...) value)) ... value)]
[value triv
       (let ([x value] ...) value)
       (if value value value)
       (call value value ...)]
[triv  x fixnum #t #f empty (void) (error uint8) ascii-char-literal]
[x     name? prim-f]
[prim-f binop unop]
[binop  * + - eq? < <= > >=]
[unop   fixnum? boolean? empty? void? ascii-char? error? not]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
[fixnum int61?]
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
      (with-handlers ([exn:fail:contract?
                       (λ (e)
                         (eprintf "Dynamic type error in ~a~n  ~a~n"
                                  who
                                  (exn-message e))
                         (error 255))])
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
[p     (module (define label (lambda (aloc ...) value)) ... value)]
[value triv
       (call value value ...)
       (let ([aloc value] ...) value)
       (if value value value)]
[triv  label aloc prim-f fixnum #t #f empty (void) (error uint8) ascii-char-literal]
[prim-f binop unop]
[binop  * + - < eq? <= > >=]
[unop   fixnum? boolean? empty? void? ascii-char? error? not]
[aloc aloc?]
[label label?]
[uint8 uint8?]
[ascii-char-literal ascii-char-literal?]
[fixnum int61?]
]

(define (interp-exprs-unique-lang-v7 x)
  (interp-exprs-lang-v7 x))

@define-grammar/pred[exprs-unsafe-data-lang-v7
#:literals (aloc? label? int64? int61? uint8? ascii-char-literal?)
#:datum-literals (module lambda define apply let if begin void error
                         >= fixnum? boolean? empty? void? ascii-char? error? not
                         call
                         unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx<
                         unsafe-fx<= unsafe-fx> unsafe-fx>= true false)
[p     (module (define label (lambda (aloc ...) value)) ... value)]
;; Do I want these in this language? I think not
#;[pred  value
       (true)
       (false)
       (not pred)
       (let ([aloc value] ...) pred)
       (if pred pred pred)]
[value triv
       (primop value ...)
       (call value value ...)
       (let ([aloc value] ...) value)
       (if value value value)]
[triv  label aloc fixnum #t #f empty (void) (error uint8) ascii-char-literal]
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

(define (interp-exprs-unsafe-data-lang-v7 x)
  (interp-base x))

@define-grammar/pred[exprs-bits-lang-v7
#:literals (aloc? label? int64?)
#:datum-literals (module lambda define apply let if void error * + - = != < <= >
                         >=  bitwise-and bitwise-ior bitwise-xor
                         arithmetic-shift-right true false not call)
[p     (module (define label (lambda (aloc ...) value)) ... value)]
[pred  (relop value value)
       (true)
       (false)
       (not pred)
       (let ([aloc value] ...) pred)
       (if pred pred pred)]
[value triv
       (binop value value)
       (call value value ...)
       (let ([aloc value] ...) value)
       (if pred value value)]
[triv  label aloc int64]
[binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
[aloc aloc?]
[label label?]
[relop < <= = >= > !=]
[int64 int64?]
]

(define (interp-exprs-bits-lang-v7 x)
  (interp-base x))

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

(define (interp-exprs-bits-lang-v7/contexts x)
  (interp-base x))

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
  [aloc aloc?]
  [label label?]
  [int64 int64?]
]

(define (interp-values-bits-lang-v7 x)
  (interp-base x))

@define-grammar/pred[imp-mf-lang-v7
  #:literals (int64? label? aloc? register? fvar? info? any info/c)
  #:datum-literals (new-frames return-point define lambda module begin jump set! halt true false not if
                               * + < <= = >= > != bitwise-and bitwise-ior
                               bitwise-xor arithmetic-shift-right)
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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [aloc   aloc?]
  [label  label?]
  [int64  int64?]
]

(define (interp-imp-mf-lang-v7 x)
  (interp-base x))

@define-grammar/pred[proc-imp-cmf-lang-v7
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
          (call triv opand ...)]
  [effect (set! aloc value)
          (begin effect ... effect)
          (if pred effect effect)]
  [opand int64 aloc]
  [triv  opand label]
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop  < <= = >= > !=]
  [aloc  aloc?]
  [label  label?]
  [int64 int64?]
]

(define (interp-proc-imp-cmf-lang-v7 x)
  (interp-base x))

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
  [aloc   aloc?]
  [label  label?]
  [rloc   register? fvar?]
  [int64  int64?]
]

(define (interp-imp-cmf-lang-v7 x)
  (interp-base x))

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

(define (interp-asm-pred-lang-v7 x)
  (interp-base x))

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

(define (interp-asm-pred-lang-v7/locals x)
  (interp-base x))

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

(define (interp-asm-pred-lang-v7/undead x)
  (interp-base x))

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

(define (interp-asm-pred-lang-v7/conflicts x)
  (interp-base x))

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

(define (interp-asm-pred-lang-v7/pre-framed x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v7/framed
  #:literals (int64? label? aloc? register? fvar? info? undead-set-tree?)
  #:datum-literals (conflicts undead-out locals define module begin set! jump
                              true false not if * + - < <= = >= > != bitwise-and
                              bitwise-ior bitwise-xor arithmetic-shift-right)
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

(define (interp-asm-pred-lang-v7/framed x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v7/spilled
  #:literals (int64? label? aloc? register? fvar? info? info/c)
  #:datum-literals (assignments conflicts locals define module begin set! jump
                                true false not if * + - < <= = >= > !=
                                bitwise-and bitwise-ior bitwise-xor
                                arithmetic-shift-right)
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

(define (interp-asm-pred-lang-v7/spilled x)
  (interp-base x))

@define-grammar/pred[asm-pred-lang-v7/assignments
  #:literals (int64? label? aloc? register? fvar? info? info/c)
  #:datum-literals (assignment locals define module begin set! jump
                               true false not if * + - < <= = >= > !=
                               bitwise-and bitwise-ior bitwise-xor
                               arithmetic-shift-right)
  [p    (module info (define label info tail) ... tail)]
  [info   #:with-contract
          (info/c
           (assignment ((aloc loc) ...)))
          (let ([loc? (or/c register? fvar?)])
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

(define (interp-asm-pred-lang-v7/assignments x)
  (interp-base x))

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

(define (interp-nested-asm-lang-fvars-v7 x)
  (interp-base x))

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

(define (interp-nested-asm-lang-v7 x)
  (interp-base x))

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

(define (interp-block-pred-lang-v7 x)
  (interp-base x))

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

(define (interp-block-asm-lang-v7 x)
  (interp-base x))

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

(define (interp-para-asm-lang-v7 x)
  (interp-para-asm-lang-v6 x))

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
  [binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
  [relop < <= = >= > !=]
  [int64 int64?]
  [int32 int32?]
  [dispoffset dispoffset?]
  [label label?]
]

(define (interp-paren-x64-v7 x)
  (interp-paren-x64-v6 x))
