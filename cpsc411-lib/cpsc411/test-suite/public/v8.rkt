#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib
 racket/dict
 "../../langs/v8.rkt"
 "../../langs/v7.rkt"
 "../utils.rkt"
 "v7.rkt")

(provide (all-defined-out))

(for ([(new-v alias-v)
       (in-dict
        `(
          (,interp-exprs-lang-v8 . ,interp-exprs-lang-v7)
          (,interp-exprs-unique-lang-v8 . ,interp-exprs-unique-lang-v7)
          (,interp-exprs-unsafe-data-lang-v8 . ,interp-exprs-unsafe-data-lang-v7)
          (,interp-exprs-bits-lang-v8 . ,interp-exprs-bits-lang-v7)
          (,interp-values-bits-lang-v8 . ,interp-values-bits-lang-v7)
          (,interp-imp-mf-lang-v8 . ,interp-imp-mf-lang-v7)
          (,interp-proc-imp-cmf-lang-v8 . ,interp-proc-imp-cmf-lang-v7)
          (,interp-imp-cmf-lang-v8 . ,interp-imp-cmf-lang-v7)

          (,interp-asm-pred-lang-v8 . ,interp-asm-pred-lang-v7)
          (,interp-asm-pred-lang-v8/locals . ,interp-asm-pred-lang-v7/locals)
          (,interp-asm-pred-lang-v8/undead . ,interp-asm-pred-lang-v7/undead)
          (,interp-asm-pred-lang-v8/conflicts . ,interp-asm-pred-lang-v7/conflicts)
          (,interp-asm-pred-lang-v8/pre-framed . ,interp-asm-pred-lang-v7/pre-framed)
          (,interp-asm-pred-lang-v8/framed . ,interp-asm-pred-lang-v7/framed)
          (,interp-asm-pred-lang-v8/spilled . ,interp-asm-pred-lang-v7/spilled)
          (,interp-asm-pred-lang-v8/assignments . ,interp-asm-pred-lang-v7/assignments)
          (,interp-nested-asm-lang-fvars-v8 . ,interp-nested-asm-lang-fvars-v7)
          (,interp-nested-asm-lang-v8 . ,interp-nested-asm-lang-v7)
          (,interp-block-pred-lang-v8 . ,interp-block-pred-lang-v7)
          (,interp-block-asm-lang-v8 . ,interp-block-asm-lang-v7)
          #;(,interp-para-asm-lang-v8 . ,interp-para-asm-lang-v7)
          #;(,interp-paren-x64-v8 . ,interp-paren-x64-v7)))])
  (register-test-programs! new-v (hash-ref test-prog-dict alias-v '())))

(register-test-programs!
 interp-exprs-bits-lang-v8
 `(("" (module
           (if (let ([x.1 (alloc 8)]
                     [y.1 (alloc 16)]
                     [z.1 0])
                 (begin
                   (mset! x.1 0 (alloc (let ([t.1 32]) (+ t.1 (+ t.1 8)))))
                   (mset! y.1 z.1 18)
                   (mset! y.1 (+ z.1 8) 40)
                   (= (mref y.1 z.1) (mref y.1 (+ z.1 8)))))
               8
               16)))))

(register-test-programs!
 interp-imp-mf-lang-v8
 '((""
    (module
        (define L.addup.1
          (lambda ()
            (begin
              (set! y.1 (alloc 16))
              (mset! y.1 8 (begin (set! x.2 8) (set! x.3 16) (+ x.2 x.3)))
              (mref y.1 8))))
      (call L.addup.1)))))

(register-test-programs!
 interp-imp-cmf-lang-v8
 '(("Mset! with a int64 operand"
    (module ((new-frames ()))
        (begin (set! sz.0 16)
               (set! x.8 (alloc sz.0))
               (mset! x.8 0 2147483648)
               (mset! x.8 8 8)
               (set! y.1 (mref x.8 0))
               (set! i.0 8)
               (set! y.2 (mref x.8 i.0))
               (set! rax (+ y.1 y.2))
               (jump r15))))))

(register-test-programs!
 interp-asm-alloc-lang-v8
 '(("Support alloc in begin if"
    (module
        ((new-frames ()))
        (begin
          (set! y.1 8)
          (set! x.1 0)
          (if (true)
              (set! x.1 (alloc y.1))
              (set! x.1 32))
          (set! rax y.1)
          (jump r15))))))

(register-test-programs!
 interp-nested-asm-lang-fvars-v8
 '((""
    (module
        (begin
          (set! rax 0)
          (begin
            (set! rbx 5)
            (if (< rax 2) (set! rbx 10) (set! rbx 9))
            (set! rax 6))
          (if (not (begin (set! fv2 10)
                          (if (< rax rbx) (not (< rax rbx))
                              (< rax rcx))))
              (begin
                (set! rax 8)
                (jump r15))
              (begin
                (set! rax 16)
                (jump r15))))))))

(register-test-programs!
 interp-exprs-lang-v8
 '((""
    (module (call cons 7 empty)))

   (""
    (module (call car (call cons 7 empty))))

   (""
    (module (call cdr (call cons 7 empty))))

   (""
    (module (call cons (call * 7 8) empty)))

   (""
    (module (call cons (if (call eq? 7 8) (call * 7 8) (call * 8 7)) empty)))

   (""
    (module
      (call make-vector 0)))

   (""
    (module
      (call make-vector 2)))

   (""
    (module
      (call vector-ref (call make-vector 2) 0)))
   ))

(define (v8-public-invalid-test-suite pass-ls interp-ls)
  (define run/read (current-run/read))
  (define passes (current-pass-list))

  (test-suite
   "v8 public test suite, error programs"
   #:before
   (thunk
    (current-pass-list pass-ls)
    (current-run/read nasm-run/exit-code))
   #:after
   (thunk
    (current-pass-list passes)
    (current-run/read run/read))

   (test-pred
    ""
    (and/c integer? (not/c zero?))
    (execute
     `(module (call car 7))
     nasm-run/exit-code))

   (test-pred
    ""
    (and/c integer? (not/c zero?))
    (execute `(module (call make-vector #\x)) nasm-run/exit-code))

   ))

(define (v8-public-test-suite pass-ls interp-ls)
  (define run/read (current-run/read))
  (define passes (current-pass-list))

  (test-suite
   "v8 public test suite"
   #:before
   (thunk
    (current-pass-list pass-ls)
    (current-run/read nasm-run/read))
   #:after
   (thunk
    (current-pass-list passes)
    (current-run/read run/read))

   (test-suite
    "compiler testomatic test suite"
    (compiler-testomatic pass-ls interp-ls nasm-run/read))))
