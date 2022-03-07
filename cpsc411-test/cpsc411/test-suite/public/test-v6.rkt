#lang racket
(require
 cpsc411/langs/v6
 rackunit)

;; check the new-frame vars get implemented and allocated correctly in the
;; interpreter.
(check-equal?
 (interp-imp-cmf-lang-v6
 '(module
      ((new-frames ((nfv.3 nfv.4))))
      (define L.many-input.1
        ((new-frames ()))
        (begin
          (set! tmp-ra.1 r15)
          (begin
            (set! x.0 rdi)
            (set! x.1 rsi)
            (set! x.2 rdx)
            (set! x.3 rcx)
            (set! x.4 r8)
            (set! x.5 r9)
            (set! x.6 fv0)
            (set! x.7 fv1)
            (begin (set! rax x.7) (jump tmp-ra.1 rbp rax)))))
    (begin
      (set! tmp-ra.2 r15)
      (begin
        (begin
          (return-point L.rp.1
            (begin
              (set! nfv.4 8)
              (set! nfv.3 7)
              (set! r9 6)
              (set! r8 5)
              (set! rcx 4)
              (set! rdx 3)
              (set! rsi 2)
              (set! rdi 1)
              (set! r15 L.rp.1)
              (jump L.many-input.1 rbp r15 rdi rsi rdx rcx r8 r9 nfv.3 nfv.4)))
          (set! res.0 rax))
        (begin (set! rax res.0) (jump tmp-ra.2 rbp rax))))))
 8)
