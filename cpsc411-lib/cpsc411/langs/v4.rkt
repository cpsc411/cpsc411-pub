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

(define interp-values-lang-v4 interp-values-lang-v3)

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

(define interp-values-lang-unique-v4 interp-values-lang-v4)

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

(define interp-imp-mf-lang-v4 interp-values-lang-unique-v4)

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

(define interp-imp-cmf-lang-v4 interp-imp-mf-lang-v4)

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

(define interp-asm-pred-lang-v4 interp-base)

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

(define interp-asm-pred-lang-v4/locals interp-asm-pred-lang-v4)

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

(define interp-asm-pred-lang-v4/undead interp-asm-pred-lang-v4/locals)

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

(define interp-asm-pred-lang-v4/conflicts interp-asm-pred-lang-v4/undead)

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

(define interp-asm-pred-lang-v4/assignments interp-asm-pred-lang-v4/conflicts)

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

(define interp-nested-asm-lang-v4 interp-values-lang-v3)

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

(module block-langs racket/base
  (require
   "base.rkt"
   (only-in racket/base [module+ r:module+] [define r:define]))
  (provide
   (all-from-out "base.rkt")
   (all-defined-out))

  (define-syntax-rule (jump l)
    (l))

  (r:module+ interp
    (provide interp-block-pred-lang-v4)
    (define-namespace-anchor a)
    (r:define interp-block-pred-lang-v4
      (let ([ns (namespace-anchor->namespace a)])
        (lambda (x) (eval x ns))))))

(require (submod 'block-langs interp))
(provide interp-block-pred-lang-v4)

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

(define interp-block-asm-lang-v4 interp-block-pred-lang-v4)

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

(module label-langs racket/base
  (require
   (submod ".." block-langs)
   (for-syntax
    racket/base
    syntax/parse)
   (only-in racket/base
            [module+ r:module+]
            [define r:define]
            [begin r:begin]
            [lambda r:lambda]))
  (provide
   (all-from-out (submod ".." block-langs))
   (all-defined-out))

  (r:define
    flags
    (make-hash
     (list
      (cons != #f)
      (cons = #f)
      (cons < #f)
      (cons <= #f)
      (cons > #f)
      (cons >= #f))))

  (define-syntax-rule (compare v1 v2)
    (for-each (lambda (cmp)
                (hash-set! flags cmp (cmp v1 v2)))
              (list != = < <= > >=)))

  (define-syntax-rule (jump-if flag d)
    (r:begin
     (when (hash-ref flags flag)
       (d))))

  (begin-for-syntax
    (define (labelify-begin defs ss)
      (syntax-parse ss
        #:datum-literals (with-label)
        [((with-label l s) ss ...)
         (let-values ([(defs e) (labelify-begin defs #`(s ss ...))])
           (values
            (cons #`(l (r:lambda () #,e)) defs)
            #`(r:begin (l))))]
        [(s ss ...)
         (if (null? (attribute ss))
             (values
              defs
              #`(r:begin s))
             (let-values ([(defs e) (labelify-begin defs (attribute ss))])
               (values
                defs
                #`(r:begin s #,e))))])))

  (define-syntax (begin stx)
    (let-values ([(defs e) (labelify-begin '() (cdr (syntax->list stx)))])
      (datum->syntax
       stx
       (syntax->datum
        #`(r:begin
           (let/ec done
             (letrec (#,@defs
                      [halt (r:lambda (v) (set! rax v) (done))])
               (r:begin
                #,e
                (done))))
           rax)))))

  (module+ interp
    (provide interp-para-asm-lang-v4)
    (define-namespace-anchor a)
    (r:define interp-para-asm-lang-v4
      (let ([ns (namespace-anchor->namespace a)])
        (r:lambda (x)
          (eval x ns))))))

(require (submod 'label-langs interp))
(provide interp-para-asm-lang-v4)

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


(define interp-paren-x64-fvars-v4 interp-para-asm-lang-v4)

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
  (interp-paren-x64-fvars-v4 `(begin ,x (halt rax))))

@(module+ test
   (paren-x64-v4?
    '(begin
       (set! rax 5))))
