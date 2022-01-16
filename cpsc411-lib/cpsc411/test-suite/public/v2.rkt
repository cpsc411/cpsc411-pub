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
  `((""
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
    #;(""
     (module
       ()
       (begin (set! x.1 undefined.1)
              (halt undefined.2))))
    (""
     (module
       ()
       (begin
         (set! x.1 0)
         (set! w.1 0)
         (set! y.1 x.1)
         (set! w.1 (+ w.1 x.1))
         (set! w.1 (+ w.1 y.1))
         (halt w.1))))))

(hash-set! test-prog-dict interp-asm-lang-v2
           (list->mutable-set asm-lang-v2-progs))

(define asm-lang-v2/locals-progs
  `((""
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
    (""
     (module
       ((locals (x.1 undefined.1 undefined.2)))
       (begin (set! x.1 undefined.1)
              (halt undefined.2))))
    (""
     (module
       ((locals (w.1 y.1 x.1)))
       (begin
         (set! x.1 0)
         (set! w.1 0)
         (set! y.1 x.1)
         (set! w.1 (+ w.1 x.1))
         (set! w.1 (+ w.1 y.1))
         (halt w.1))))))

(hash-set! test-prog-dict interp-asm-lang-v2/locals
           (list->mutable-set asm-lang-v2/locals-progs))
(define asm-lang-v2/assignments-progs
  `((""
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
    (""
     (module
       ((locals (x.1 y.1 z.1))
        (assignment ((x.1 fv0) (y.1 fv1) (z.1 fv2))))
       (begin (set! x.1 1)
              (set! y.1 x.1)
              (set! y.1 (+ y.1 1))
              (set! z.1 y.1)
              (set! z.1 (+ z.1 1))
              (halt z.1))))
    (""
     (module
       ((locals (x.1 undefined.1 undefined.2))
        (assignment ((x.1 fv0) (undefined.1 fv1) (undefined.2 fv2))))
       (begin (set! x.1 undefined.1)
              (halt undefined.2))))
    (""
     (module
       ((locals (w.1 y.1 x.1))
        (assignment ((w.1 fv0) (y.1 fv1) (x.1 fv2))))
       (begin
         (set! x.1 0)
         (set! y.1 x.1)
         (set! w.1 (+ w.1 x.1))
         (set! w.1 (+ w.1 y.1))
         (halt w.1))))))

(hash-set! test-prog-dict interp-asm-lang-v2/assignments
           (list->mutable-set asm-lang-v2/assignments-progs))

(define nested-asm-lang-v2-progs
  `((""
     (begin (halt 5)))
    (""
     (begin (begin (halt 5))))
    (""
     (begin (begin (set! fv0 1)
                   (set! fv1 2))
            (set! fv0 (+ fv0 fv1))
            (halt fv0)))
    (""
     (begin (begin (set! fv0 1)
                   (set! fv1 2))
            (begin (begin (set! fv0 (+ fv0 fv1))))
            (halt fv0)))))

(hash-set! test-prog-dict interp-nested-asm-lang-v2
           (list->mutable-set nested-asm-lang-v2-progs))

(define para-asm-lang-v2-progs
  `((""
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
    (""
     (begin
       (set! fv0 0)
       (set! fv0 (+ fv0 1))
       (halt 1)))
    (""
     (begin
       (set! fv0 0)
       (set! fv1 0)
       (set! fv0 (+ fv0 fv1))
       (halt 1)))
    (""
     (begin
       (set! fv1 7)
       (set! fv2 0)
       (set! r8 5)
       (set! r12 fv1)
       (set! r12 (+ r12 r8))
       (set! fv2 (+ fv2 4))
       (halt fv2)))
    (""
     (begin
       (set! rbx 0)
       (set! rcx 0)
       (set! r9 42)
       (set! rbx (+ rbx rcx))
       (set! rbx (+ rbx r9))
       (halt rbx)))
    (""
     (begin
       (set! fv0 2)
       (set! fv1 5)
       (set! fv0 (+ fv0 fv1))
       (halt fv0)))
    ("Set fvar to max int64 val"
     (begin (set! fv0 2147483648) (halt 12)))))

(hash-set! test-prog-dict interp-para-asm-lang-v2
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
    (""
     (begin
       (set! fv1 1)
       (set! fv0 2)
       (set! rax 1)
       (set! rax (+ rax fv1))
       (set! rax (* rax fv0))))
    (""
     (begin
       (set! r11 0)
       (set! fv0 2)
       (set! fv1 0)
       (set! rsi r11)
       (set! rsi (+ rsi fv0))
       (set! rsi (+ rsi 0))
       (set! rdi rsi)
       (set! rdi (+ rdi fv1))
       (set! rax rdi)))
    (""
     (begin
       (set! fv5 2)
       (set! rax fv5)
       (set! rax (+ rax fv5))
       (set! fv5 rax)
       (set! rax fv5)))))

(hash-set! test-prog-dict interp-paren-x64-fvars-v2
           (list->mutable-set paren-x64-fvars-v2-progs))

(define paren-x64-v2-progs
  (append
   paren-x64-v1-progs
   `((""
      (begin
        (set! (rbp - 0) 1)
        (set! rax (rbp - 0))))
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
        (set! (rbp - 8) 1)
        (set! (rbp - 0) 2)
        (set! rax 1)
        (set! rax (+ rax (rbp - 8)))
        (set! rax (* rax (rbp - 0)))))
     (""
      (begin
        (set! r11 0)
        (set! (rbp - 0) 2)
        (set! (rbp - 8) 0)
        (set! rsi r11)
        (set! rsi (+ rsi (rbp - 0)))
        (set! rsi (+ rsi 0))
        (set! rdi rsi)
        (set! rdi (+ rdi (rbp - 8)))
        (set! rax rdi)))
     (""
      (begin
        (set! r9 0)
        (set! r12 0)
        (set! r9 (* r9 r12))
        (set! r9 (* r9 0))
        (set! r9 (+ r9 1))
        (set! rcx r9)
        (set! rcx (* rcx 1))
        (set! rax 7)
        (set! rax (+ rax rcx))
        (set! rax (+ rax rax))
        (set! r14 2)
        (set! r14 (+ r14 rax))
        (set! rax (* rax r14))))
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
        (set! rax rdi))))))


(hash-set! test-prog-dict interp-paren-x64-v2
           (list->mutable-set paren-x64-v2-progs))
