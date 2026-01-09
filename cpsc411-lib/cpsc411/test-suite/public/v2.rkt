#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/set
 racket/match
 racket/function
 racket/list
 rackunit
 "../utils.rkt"
 "../../langs/v2.rkt"
 "v1.rkt"
 cpsc411/compiler-lib)

(provide
 (all-defined-out))

(define asm-lang-v2-progs
  ;; string? x asm-lang-v2?
  ;; a name and a test program
  ;; valued define by langs/v1 interpreter
  `(
    (""
     (module () (halt 5)))

    (""
     (module ()
       (begin (set! x.1 5)
              (halt x.1))))
    (""
     (module
       ()
       (begin (set! x.1 1)
              (set! y.1 x.1)
              (set! y.1 (+ y.1 1))
              (set! z.1 y.1)
              (set! z.1 (+ z.1 1))
              (halt z.1))))
  ))

(register-test-programs!
 interp-asm-lang-v2
 (list->mutable-set asm-lang-v2-progs))

(define asm-lang-v2/locals-progs
  `(
    (""
     (module
       ((locals ()))
       (halt 5)))

    (""
     (module
       ((locals (x.1)))
       (begin (set! x.1 5)
              (halt x.1))))

    (""
     (module
       ((locals (x.1 y.1 z.1)))
       (begin (set! x.1 1)
              (set! y.1 x.1)
              (set! y.1 (+ y.1 1))
              (set! z.1 y.1)
              (set! z.1 (+ z.1 1))
              (halt z.1))))
    ))

(register-test-programs!
 interp-asm-lang-v2/locals
 (list->mutable-set asm-lang-v2/locals-progs))

(define asm-lang-v2/assignments-progs
  `(
    (""
     (module
       ((locals ())
        (assignment ()))
       (halt 5)))

    (""
     (module
       ((locals (x.1))
        (assignment ((x.1 fv0))))
       (begin (set! x.1 5)
              (halt x.1))))

    ))

(register-test-programs!
 interp-asm-lang-v2/assignments
 (list->mutable-set asm-lang-v2/assignments-progs))

(define nested-asm-lang-v2-progs
  `((""
     (begin (halt 5)))
    (""
     (begin (begin (halt 5))))
    ))

(register-test-programs!
 interp-nested-asm-lang-v2
 (list->mutable-set nested-asm-lang-v2-progs))

(define para-asm-lang-v2-progs
  `(
    (""
     (begin (halt 5)))

    (""
     (begin
       (set! fv0 5)
       (halt fv0)))

    (""
     (begin
       (set! fv1 6)
       (set! fv0 fv1)
       (halt 5)))

    (""
     (begin
       (set! r12 0)
       (set! r8 0)
       (set! r12 (+ r12 r8))
       (halt 1)))

    ))

(register-test-programs!
 interp-para-asm-lang-v2
 (list->mutable-set para-asm-lang-v2-progs))

(define paren-x64-fvars-v2-progs
  `((""
     (begin
       (set! rax 5)))
    #;(""
     (begin
       (set! rax fv0)))
    #;(""
     (begin
       (set! rax fv3)))
    (""
     (begin
       (set! fv1 1)
       (set! rax 1)))
    (""
     (begin
       (set! rax 1)
       (set! fv1 rax)))
    ))

(register-test-programs!
 interp-paren-x64-fvars-v2
 (list->mutable-set paren-x64-fvars-v2-progs))

(define paren-x64-v2-progs
  (append
   paren-x64-v1-progs
   `(
     (""
      (begin
        (set! (rbp - 8) 1)
        (set! rax (rbp - 8))))

     (""
      (begin
        (set! (rbp - 24) 24)
        (set! rax (rbp - 24))))

     (""
      (begin
        (set! rax 1)
        (set! (rbp - 8) rax)))

     (""
      (begin
        (set! r11 0)
        (set! r8 2)
        (set! rcx 0)
        (set! rsi r11)
        (set! rsi (+ rsi r8))
        (set! rsi (+ rsi 0))
        (set! rdi rsi)
        (set! rdi (+ rdi rcx))
        (set! rax rdi)))
     )))


(register-test-programs!
 interp-paren-x64-v2
 (list->mutable-set paren-x64-v2-progs))

(define (v2-public-test-sutie pass-ls interp-ls)
  (define run/read (current-run/read))
  (define old-pass-ls (current-pass-list))

  (test-suite
   "v2 public test suite"
   #:before
   (thunk
    (current-run/read nasm-run/print-number)
    (current-pass-list pass-ls))
   #:after
   (thunk
    (current-run/read run/read)
    (current-pass-list old-pass-ls))

   (compiler-testomatic pass-ls interp-ls)))
