#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib
 racket/dict
 "../../langs/v4.rkt"
 "../../langs/v3.rkt"
 "../../langs/v2.rkt"
 "../utils.rkt"
 "v1.rkt"
 "v2.rkt"
 "v3.rkt")

(provide (all-defined-out))

(for ([(new-v alias-v)
       (in-dict
        `((,interp-values-lang-v4 . ,interp-values-lang-v3)
          (,interp-values-unique-lang-v4 . ,interp-values-unique-lang-v3)
          (,interp-imp-mf-lang-v4 . ,interp-imp-mf-lang-v3)
          (,interp-imp-cmf-lang-v4 . ,interp-imp-cmf-lang-v3)
          (,interp-asm-pred-lang-v4 . ,interp-asm-lang-v2)
          (,interp-asm-pred-lang-v4/locals . ,interp-asm-lang-v2/locals)
          (,interp-asm-pred-lang-v4/assignments . ,interp-asm-lang-v2/assignments)
          ;; needs update
          #;(,interp-nested-asm-lang-v4 . ,interp-nested-asm-lang-v2)
          (,interp-para-asm-lang-v4 . ,interp-para-asm-lang-v2)
          (,interp-paren-x64-fvars-v4 . ,interp-paren-x64-fvars-v2)
          (,interp-paren-x64-v4 . ,interp-paren-x64-v2)))])
  (register-test-programs!
   new-v
   (set->list (hash-ref test-prog-dict alias-v))))

(register-test-programs!
 interp-nested-asm-lang-v4
 (for/list ([p nested-asm-lang-v2-progs])
   `(,(first p) (module ,(second p)))))

(register-test-programs!
 interp-values-lang-v4
 '((""
    (module
      (let ([y 200])
        (if (< 3 y) 1 0))))

   (""
    (module
      (let ([x 1])
        (let ([y (let ([z 3]) z)])
          (let ([z (let ([y 2]) (+ y y))])
            (if (let ([x 6]) (> x 7)) 9 10))))))

   (""
    (module
      (if (true) ; (not (true))
          (if (let ([y 11] [z 15])
                (> y z))
              14
              15)
          (let ([z 12])
            (let ([z 15] [y 1])
              (+ z y))))))

   (""
    (module
      (let ([x 3] [y 5]) (+ y x))))

   (""
    (module
      (let ([x 3]
            [y 5]
            [z (let ([q (* 4 2)]
                     [r (+ 5 6)])
                 (+ q r))])
        (let ([m (+ x y)])
          (* m z)))))

   (""
    (module
      (let ([x 1]
            [y (let ([z 3]) z)]
            [z (let ([y 2]) (+ y y))])
        (+ y z))))


   (""
    (module
      (let ([x 20] [y 21])
        (if (not (> x 12))
            (if (if (let ([z x])
                      (< y z))
                    (true)
                    (false))
                10
                12)
            (+ x y)))))

   (""
    (module
      (let ([x 1]
            [y (let ([z 3]) z)]
            [z (let ([y 2]) (+ y y))])
        (+ y z))))

   (""
    (module (if (= 0 0) 0 1)))))

(register-test-programs!
 interp-imp-mf-lang-v4
 '((""
    (module
      (begin
        (set! x.37 1)
        (begin
          (set! x.37 x.37)
          (set! y.39 (+ x.37 x.37))
          (set! z.38 (begin (set! x.37 8) x.37))
          (if (true)
              (begin (set! z.38 (+ x.37 7)) 8)
              9)))))

   (""
    (module
      (begin
        (if (true) (set! x.3 (begin (set! y.4 (begin (set! z.4 (+ 4 5))
                                                     z.4))
                                    y.4))
            (set! x.3 y.7))
        x.3)))

   (""
    (module
      (begin
        (set! x.5 (if (true)
                      (begin (set! y.2 14) 12)
                      (begin 15)))
        x.5)))

   (""
    (module
      (begin
        (set! x.1 0)
        (set! x.5 (if (true)
                      (begin (set! y.2 (+ x.1 17)) 12)
                      (begin 15)))
        x.5)))


   (""
    (module
      (begin
        (set! x.3 (begin (set! y.4 (begin (set! z.4 (+ 4 5))
                                          z.4))
                         y.4))
        x.3)))

   (""
    (module
      (begin
        (set! x.6 2)
        (set! x.5 1)
        (begin
          (set! x.4 x.5)
          (set! x.6
                (begin
                  (set! x.4 2) x.4))
          (+ 2 3)))))


   (""
    (module
      (begin
        (set! x.6 (+ 2 3))
        (set! x.7 (+ x.6 x.6))
        (begin
          (set! y.2 5)
          (+ x.6 y.2)))))))

(register-test-programs!
 interp-imp-cmf-lang-v4
 '((""
    (module
      (begin
        (set! x.1 5)
        (if (true)
            (begin (set! y.2 (+ x.1 17)) (set! x.5 12))
            (begin (set! x.5 15)))
        x.5)))

   (""
    (module
      (begin
        (set! x.1 10)
        (set! y.3 7)
        (set! x.1 (+ x.1 y.3))
        (set! x.1 (+ y.3 y.3))
        x.1)))

   (""
    (module
      (begin
        (set! y.4 5)
        (set! x.3 (+ 1 2))
        (set! z.5 2)
        (set! x.3 (+ x.3 y.4))
        (set! x.3 (+ y.4 z.5))
        (set! x.3 x.3)
        x.3)))

   (""
    (module
      (begin
        (set! x.1 (+ 4 10))
        (+ x.1 5))))

   (""
    (module (if (> 0 1)
                0
                (+ 0 1))))))

(register-test-programs!
 interp-asm-pred-lang-v4
 '((""
    (module ()
            (begin
              (set! x.1 0)
              (set! w.1 0)
              (set! y.1 x.1)
              (set! w.1 (+ w.1 x.1))
              (set! w.1 (+ w.1 y.1))
              (halt w.1))))))

(register-test-programs!
 interp-asm-pred-lang-v4/locals
 '((""
    (module ((locals (x.1 y.2 b.3 c.4)))
            (begin
              (set! x.1 5)
              (set! y.2 x.1)
              (begin
                (set! b.3 x.1)
                (set! b.3 (+ b.3 y.2))
                (set! c.4 b.3)
                (if (= c.4 b.3)
                    (halt c.4)
                    (begin
                      (set! x.1 c.4)
                      (halt c.4)))))))))


(register-test-programs!
 interp-asm-pred-lang-v4/undead
 '((""
    (module
      ((locals (x.1 y.2))
       (undead-out
        ((x.1) (x.1 y.2) ((x.1 y.2) () ()))))
      (begin (set! x.1 3) (set! y.2 x.1) (if (> y.2 x.1) (halt x.1) (halt y.2)))))))

(register-test-programs!
 interp-asm-pred-lang-v4/assignments
 '((""
    (module
      ((locals (w.1 y.1 x.1))
       (assignment ((w.1 fv0) (y.1 fv8) (x.1 fv16))))
      (begin
        (set! x.1 0)
        (set! w.1 0)
        (set! y.1 x.1)
        (set! w.1 (+ w.1 x.1))
        (set! w.1 (+ w.1 y.1))
        (halt w.1))))))

(register-test-programs!
 interp-nested-asm-lang-v4
 '((""
    (module
      (begin
        (set! r8 0)
        (set! r9 0)
        (if (not (if (true)
                     (> r8 5)
                     (< r9 6)))
            (set! r9 15)
            (set! r12 90))
        (halt r12))))

   (""
    (module
      (begin
        (set! r8 12)
        (set! r9 12)
        (if (not (true)) (set! r12 15) (set! r13 90))
        (halt r8))))

   (""
    (module
      (begin
        (set! r8 12)
        (set! r9 12)
        (if (true) (set! r12 15) (set! r13 90))
        (halt r8))))

   (""
    (module
      (begin
        (set! fv1 12)
        (set! fv2 12)
        (halt fv3))))

   ))

(register-test-programs!
 interp-block-pred-lang-v4
 '((""
    (module
      (define L.main.1
        (begin (set! rbx 1)
               (set! rcx 2)
               (if (not (not (true)))
                   (jump L.t.1)
                   (jump L.t.2))))
      (define L.t.1
        (begin (set! rdx 4)
               (halt rdx)))
      (define L.t.2
        (begin (set! rdx 8)
               (halt rdx)))))))

(register-test-programs!
 interp-para-asm-lang-v4
 '((""
    (begin
      (set! fv1 7)
      (set! r8 5)
      (set! r12 fv1)
      (set! fv2 0)
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

   (""
    (begin
      (set! fv0 0)
      (set! fv1 1)
      (compare fv0 fv1)
      (jump-if > L.foo.1)
      (halt 0)
      (with-label L.foo.1 (halt 1))))

   (""
    (begin
      (set! rax 0)
      (set! fv0 1)
      (compare rax fv0)
      (halt 12)))))

(register-test-programs!
 interp-paren-x64-fvars-v4
 '((""
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

(register-test-programs!
 interp-paren-x64-v4
 '((""
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
      (set! rbp 2)
      (set! rcx 0)
      (set! rsi r11)
      (set! rsi (+ rsi rbp))
      (set! rsi (+ rsi 0))
      (set! rdi rsi)
      (set! rdi (+ rdi rcx))
      (set! rax rdi)))

   ("Simple label case"
    (begin
      (set! rdx 4)
      (with-label L.x.1 (set! rdx (+ rdx -111)))
      (set! rax rdx)))

   ("Moderate label case"
    (begin
      (set! (rbp - 0) 21)
      (set! rax 0)
      (set! rsp 1)
      (with-label L.test.1 (set! (rbp - 8) 2))
      (compare rax rsp)
      (jump-if = L.test.1)
      (set! rax (rbp - 0))
      (set! rax (* rax (rbp - 8)))))

   ("Complex case"
    (begin
      (set! rdx 4)
      (set! rcx L.testing.4)
      (with-label L.testing.1 (jump L.testing.3))
      (with-label L.testing.2 (jump L.testing.1))
      (with-label L.testing.3 (jump rcx))
      (with-label L.testing.4 (set! rax rdx))))

   ("Simple single label case with jump"
    (begin
      (set! rdi L.label.1)
      (set! rax 1)
      (jump rdi)
      (set! rax 10)
      (with-label L.label.1
        (set! rax (+ rax 100)))))

   ("Moderate single label case with jump"
    (begin
      (set! rbx 4)
      (compare rbx 4)
      (jump-if < L.testing.1)
      (set! rbx -66)
      (jump L.end.1)
      (with-label L.testing.1 (set! rbx 11))
      (with-label L.end.1 (set! rax rbx))
      (set! rax rax)))

   ("Moderate single label case with jump"
    (begin
      (set! (rbp - 16) -1)
      (set! r10 0)
      (set! rax 10)
      (with-label L.x.1 (set! rax (+ rax (rbp - 16))))
      (compare rax r10)
      (jump-if > L.x.1)))

   ("Complex case"
    (begin
      (with-label L.cs411main.1
        (set! r11 7))
      (with-label L.fib.1
        (set! r9 0))
      (set! r10 1)
      (set! r12 r11)
      (with-label L.fib_iter.1
        (compare r12 0))
      (jump-if = L.fib_done.1)
      (with-label L.fib_not_done_yet.1
        (set! r13 r10))
      (set! r10 (+ r10 r9))
      (set! r9 r13)
      (set! r12 (+ r12 -1))
      (jump L.fib_iter.1)
      (with-label L.fib_done.1
        (set! rdi r10))
      (set! rax rdi)))


   ))

(define (v4-public-test-suite pass-ls interp-ls
                              #;link-paren-x64
                              #;interp-paren-x64
                              #;interp-values-lang
                              #;check-values-lang)
  (define run/read (current-run/read))
  (define passes (current-pass-list))

  (test-suite
   "v4 public test suite"
   #:before
   (thunk
    (current-pass-list pass-ls)
    (current-run/read nasm-run/print-number))
   #:after
   (thunk
    (current-pass-list passes)
    (current-run/read run/read))

   (test-suite
    "compiler testomatic test suite"
    (compiler-testomatic pass-ls interp-ls))))