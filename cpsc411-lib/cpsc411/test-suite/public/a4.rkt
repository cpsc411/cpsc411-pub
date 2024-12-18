#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib
 "../../langs/v4.rkt"
 )

(provide (all-defined-out))

(define-check (test-eq-normal m f e o)
  (let ([s string-normalize-spaces])
    (test-case m (check-equal? (s (f e)) (s o)))))

(define (a4-link-paren-x64-test-suite link-paren-x64)
  (test-suite
   "a4 link-paren-x64 test suite"

   (test-case "Single label case"
     (test-match
      (link-paren-x64
       '(begin
          (with-label L.test.1 (set! (rbp - 0) 8))
          (set! (rbp - 8) 0)
          (set! rax (rbp - 0))
          (set! rax (* rax (rbp - 0)))))
      '(begin
         (set! (rbp - 0) 8)
         (set! (rbp - 8) 0)
         (set! rax (rbp - 0))
         (set! rax (* rax (rbp - 0))))))

   (test-case "Single label case with jump"
     (test-match
      (link-paren-x64
       '(begin
          (with-label L.test.1 (set! r9 7))
          (jump L.test.1)))
      '(begin
         (set! r9 7)
         (jump 0))))

   (test-case "Multiple label case with jumps"
     (test-match
      (link-paren-x64
       '(begin
          (set! rsp 1)
          (with-label L.test.1 (set! rax 1))
          (set! rbx 2)
          (jump L.test.2)
          (set! rdi 2)
          (with-label L.test.2 (set! rsi 2))
          (jump L.test.1)))
      '(begin
         (set! rsp 1)
         (set! rax 1)
         (set! rbx 2)
         (jump 5)
         (set! rdi 2)
         (set! rsi 2)
         (jump 1))))

   (test-case "Complex nested label case"
     (test-match
      (link-paren-x64
       '(begin
          (set! rax L.link.1)
          (with-label L.link.1
            (jump L.link.2))
          (with-label L.link.2
            (compare rbx 3))
          (with-label L.link.3
            (jump-if = L.link.4))
          (with-label L.link.4
            (set! rax L.link.2))
          (jump L.link.1)))
      '(begin
         (set! rax 1)
         (jump 2)
         (compare rbx 3)
         (jump-if = 4)
         (set! rax 2)
         (jump 1))))))

(define (a4-interp-paren-x64-test-suite interp-paren-x64)
  (test-suite
   "a4 interp-paren-x64 test suite"

   (let ([e `(begin
               (set! rdi L.label.1)
               (set! rax 1)
               (jump rdi)
               (set! rax 10)
               (with-label L.label.1
                 (set! rax (+ rax 100))))])
     (test-case "Simple single label case with jump"
       (check-equal? (interp-paren-x64 e) 101)))

   (let ([e `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if < L.testing.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (set! rbx 11))
               (with-label L.end.1 (set! rax rbx))
               (set! rax rax))])
     (test-case "Moderate single label case with jump"
       (check-equal? (interp-paren-x64 e) -66)))

   (test-case "Moderate single label case with jump"
     (check-equal?
      (interp-paren-x64
       `(begin
          (set! (rbp - 16) -1)
          (set! r10 0)
          (set! rax 10)
          (with-label L.x.1 (set! rax (+ rax (rbp - 16))))
          (compare rax r10)
          (jump-if > L.x.1)))
      0))

   (let ([e '(begin
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
               (set! rax rdi))])
     (test-case "Complex case"
       (check-equal? (interp-paren-x64 e) 21)))))

(define (a4-generate-x64-test-suite passes interp-paren-x64 generate-x64)
  (test-suite
   "a4 generate-x64 test suite"

   (let ([x '(begin
               (set! rdx 4)
               (with-label L.x.1 (set! rdx (+ rdx -111)))
               (set! rax rdx))])
     (test-eq-normal
      "Simple label case"
      generate-x64
      x
      "mov rdx, 4
 L.x.1:
 add rdx, -111
 mov rax, rdx")

     (test-begin (check-equal? (interp-paren-x64 x) -107))

     (test-from generate-x64 passes x -107))

   (let ([x '(begin
               (set! (rbp - 0) 21)
               (set! rax 0)
               (set! rsp 1)
               (with-label L.test.1 (set! (rbp - 8) 2))
               (compare rax rsp)
               (jump-if = L.test.1)
               (set! rax (rbp - 0))
               (set! rax (* rax (rbp - 8))))])

     (test-eq-normal
      "Moderate label case"
      generate-x64
      x
      "mov QWORD [rbp - 0], 21
       mov rax, 0
       mov rsp, 1
  L.test.1:
  mov QWORD [rbp - 8], 2
  cmp rax, rsp
  je L.test.1
  mov rax, QWORD [rbp - 0]
  imul rax, QWORD [rbp - 8]")

     (test-begin (check-equal? (interp-paren-x64 x) 42))

     (test-from generate-x64 passes x 42))

   (let ([x '(begin
               (set! rdx 4)
               (set! rcx L.testing.4)
               (with-label L.testing.1 (jump L.testing.3))
               (with-label L.testing.2 (jump L.testing.1))
               (with-label L.testing.3 (jump rcx))
               (with-label L.testing.4 (set! rax rdx)))])
     (test-eq-normal
      "Complex case"
      generate-x64
      x
      "mov rdx, 4
       mov rcx, L.testing.4
L.testing.1:
jmp L.testing.3
L.testing.2:
jmp L.testing.1
L.testing.3:
jmp rcx
L.testing.4:
mov rax, rdx")

     (test-begin (check-equal? (interp-paren-x64 x) 4))

     (test-from generate-x64 passes x 4))))

(define (a4-patch-instructions-test-suite passes patch-instructions)
  (define-check (test-patch-instructions x)
    (test-correct interp-para-asm-lang-v4 interp-paren-x64-fvars-v4
                   x
                   (patch-instructions x)))
  (test-suite
   "a4 patch-instructions tests"

   (test-suite
    "Simple label case without jump"
     (let ([x '(begin
                 (set! rsi L.label.1)
                 (with-label L.label.1
                   (set! rbx 18))
                 (halt rbx))])
       (test-match
        (patch-instructions x)
        `(begin
           (set! rsi ,l1)
           (with-label ,l1
             (set! rbx 18))
           (set! rax rbx)
           (jump done)))

       (test-patch-instructions x)))


   (test-suite
    "Simple label case with jump"
     (let ([x '(begin
                 (set! rcx L.label.1)
                 (jump rcx)
                 (with-label L.label.1
                   (set! rcx 42))
                 (halt rcx))])
       (test-match
        (patch-instructions x)
        `(begin
           (set! rcx L.label.1)
           (jump rcx)
           (with-label L.label.1
             (set! rcx 42))
           (set! rax rcx)
           (jump done)))

       (test-patch-instructions x)))

   (test-suite
    "Moderate single label case"
     (let ([x '(begin
                 (set! rdx 9)
                 (set! rcx 18)
                 (set! rsi L.label.1)
                 (compare rdx rcx)
                 (jump-if < L.label.1)
                 (with-label L.label.1
                   (set! rdx 27))
                 (halt rdx))])
       (test-match
        (patch-instructions x)
        `(begin
           (set! rdx 9)
           (set! rcx 18)
           (set! rsi L.label.1)
           (compare rdx rcx)
           (jump-if < L.label.1)
           (with-label L.label.1
             (set! rdx 27))
           (set! rax rdx)
           (jump done)))

       (test-patch-instructions x)))

   ; or checks could be made more precise
   (test-suite
    "Complex multiple label case"
     (let ([x '(begin
                 (with-label L.main.51 (set! r14 1))
                 (set! r15 5)
                 (with-label L.fact_loop.50 (compare r15 0))
                 (jump-if = L.nested.54)
                 (set! r13 r15)
                 (set! r15 (+ r15 -1))
                 (set! r14 (* r14 r13))
                 (jump L.fact_loop.50)
                 (with-label L.nested.54 (halt r14)))])
       (test-match
        (patch-instructions x)
        `(begin
           (with-label L.main.51 (set! r14 1))
           (set! r15 5)
           (with-label L.fact_loop.50 (compare r15 0))
           (jump-if = L.nested.54)
           (set! r13 r15)
           (set! r15 (+ r15 -1))
           (set! r14 (* r14 r13))
           (jump L.fact_loop.50)
           (with-label L.nested.54 (set! rax r14))
           (jump done)))

       (test-patch-instructions x)))

   (test-case
       "compare register and fvar"
     (check-match
      (patch-instructions
       '(begin (compare rax fv0) (halt 12)))
      `(begin
         (set! ,reg fv0)
         (compare rax ,reg)
         (set! ,rax 12)
         (jump done))
      (and (eq? rax  (current-return-value-register))
           (memq reg (current-auxiliary-registers))
           #t)))))

(define (a4-flatten-program-test-suite passes flatten-program)
  (define-check (test-flatten-correct x)
    (test-correct interp-block-asm-lang-v4 interp-para-asm-lang-v4
                  x
                  (flatten-program x)))

  (test-suite
   "a4 flatten-program tests"

   (test-case "Simple case"
     (check-match
      (flatten-program
       '(module
          (define L.x.1
            (halt 123))))
      `(begin
         ,(or
           `(with-label L.x.1
              (halt 123))
           `(halt 123)))))

   (test-case "Simple case with jump"
     (check-match
      (flatten-program
       '(module
          (define L.x.0
            (begin
              (set! rsi 123)
              (set! rsi (+ rsi 1))
              (jump L.x.1)))
          (define L.x.1
            (halt rsi))))
      `(begin
         ,(or
           `(with-label L.x.0
              (set! rsi 123))
           `(set! rsi 123))
         (set! rsi (+ rsi 1))
         (jump L.x.1)
         (with-label L.x.1
           (halt rsi)))))

   (test-case "Simple case with two blocks"
     (check-match
      (flatten-program
       '(module
          (define L.x.1
            (begin
              (set! rcx rcx)
              (jump L.x.1)))
          (define L.x.2
            (begin
              (set! rcx rcx)
              (halt 0)))))
      `(begin
         ,(or
           `(with-label L.x.1 (set! rcx rcx))
           `(set! rcx rcx))
         (jump L.x.1)
         (with-label L.x.2 (set! rcx rcx))
         (halt 0))))

   (test-case "Complex case"
     (check-match
      (flatten-program
       `(module
          (define L.tmp.1
            (begin
              (set! rdx 42)
              (begin
                (set! rdx 20)
                (begin
                  (set! rdx (+ rdx rdx))
                  (jump L.tmp.2)))))
          (define L.tmp.2
            (begin
              (halt rdx)))))
      `(begin
         ,(or
           `(with-label L.tmp.1
              (set! rdx 42))
           `(set! rdx 42))
         (set! rdx 20)
         (set! rdx (+ rdx rdx))
         (jump L.tmp.2)
         (with-label L.tmp.2
           (halt rdx)))))

   (test-case "Complex case"
     (check-match
      (flatten-program
       `(module
          (define
            L.tmp.1
            (begin
              (set! rdx 42)
              (if (> rdx 43) (jump L.tmp.2) (jump L.tmp.3))))
          (define
            L.tmp.2
            (begin
              (set! rdx 30)
              (jump L.tmp.4)))
          (define
            L.tmp.3
            (begin
              (set! rdx 0)
              (jump L.tmp.4)))
          (define
            L.tmp.4
            (halt rdx))))
      `(begin
         ,(or
           `(with-label L.tmp.1
              (set! rdx 42))
           `(set! rdx 42))
         (compare rdx 43)
         (jump-if > L.tmp.2)
         (jump L.tmp.3)
         (with-label L.tmp.2
           (set! rdx 30))
         (jump L.tmp.4)
         (with-label L.tmp.3
           (set! rdx 0))
         (jump L.tmp.4)
         (with-label L.tmp.4
           (halt rdx)))))))

(define (a4-resolve-predicates-test-suite passes resolve-predicates)
  (define-check (test-resolve-predicates-correct source)
    (test-correct interp-block-pred-lang-v4 interp-block-asm-lang-v4 source
                   (resolve-predicates source)))

  (test-suite
   "a4 resolve-predicates tests"

   (test-suite
    "trivial predicate"
    (let ([x '(module
                (define L.main.1
                  (begin (set! rbx 1)
                         (set! rcx 2)
                         (if (true)
                             (jump L.t.1)
                             (jump L.t.2))))
                (define L.t.1
                  (begin (set! rdx 4)
                         (halt rdx)))
                (define L.t.2
                  (begin (set! rdx 8)
                         (halt rdx))))])

      (test-match
       (resolve-predicates x)
       `(module
          (define L.main.1
            (begin (set! rbx 1)
                   (set! rcx 2)
                   (jump L.t.1)))
          (define L.t.1
            (begin (set! rdx 4)
                   (halt rdx)))
          (define L.t.2
            (begin (set! rdx 8)
                   (halt rdx)))))

      (test-resolve-predicates-correct x)))

   (test-suite
    "trivial predicate"
    (let ([x '(module
                (define L.main.1
                  (begin (set! rbx 1)
                         (set! rcx 2)
                         (if (false)
                             (jump L.t.1)
                             (jump L.t.2))))
                (define L.t.1
                  (begin (set! rdx 4)
                         (halt rdx)))
                (define L.t.2
                  (begin (set! rdx 8)
                         (halt rdx))))])

      (test-match
       (resolve-predicates x)
       `(module
          (define L.main.1
            (begin (set! rbx 1)
                   (set! rcx 2)
                   (jump L.t.2)))
          (define L.t.1
            (begin (set! rdx 4)
                   (halt rdx)))
          (define L.t.2
            (begin (set! rdx 8)
                   (halt rdx)))))

      (test-resolve-predicates-correct x)))

   (test-suite
    "switching predicate"
     (let ([x '(module
                 (define L.main.1
                   (begin (set! rbx 1)
                          (set! rcx 2)
                          (if (not (< rbx rcx))
                              (jump L.t.1)
                              (jump L.t.2))))
                 (define L.t.1
                   (begin (set! rdx 4)
                          (halt rdx)))
                 (define L.t.2
                   (begin (set! rdx 8)
                          (halt rdx))))])

       (test-match
        (resolve-predicates x)
        `(module
           (define L.main.1
             (begin (set! rbx 1)
                    (set! rcx 2)
                    ,(or
                      `(if (< rbx rcx)
                        (jump L.t.2)
                        (jump L.t.1))
                      `(if (>= rbx rcx)
                           (jump L.t.1)
                           (jump L.t.2)))))
           (define L.t.1
             (begin (set! rdx 4)
                    (halt rdx)))
           (define L.t.2
             (begin (set! rdx 8)
                    (halt rdx)))))

       (test-resolve-predicates-correct x)))

   (test-case "negating constant"
     (check-equal?
      (resolve-predicates
       '(module
            (define L.start.1
              (if (not (false))
                  (jump L.start.1)
                  (jump L.start.2)))))
      '(module (define L.start.1 (jump L.start.1)))))))

(define (a4-expose-basic-blocks-test-suite passes expose-basic-blocks)
  (define-check (test-ebb-correct source)
    (test-correct interp-nested-asm-lang-v4 interp-block-pred-lang-v4 source
                   (expose-basic-blocks source)))

  (test-suite
   "a4 expose-basic-blocks tests"

   (let ([x `(module (begin (halt 5)))])
     (fragile-test-case
      (test-match (expose-basic-blocks x)
                   `(module (define ,L.main.1 (begin (halt 5))))))

     (test-ebb-correct x)

     (test-from expose-basic-blocks passes x 5))

   (let ([x `(module (begin (begin (halt 5))))])
     (fragile-test-case
      (test-match (expose-basic-blocks x)
                   `(module (define ,L.main.1 (begin (halt 5))))))

     (test-ebb-correct x)

     (test-from expose-basic-blocks passes x 5))

   (let ([x `(module
               (begin (begin (set! fv0 1)
                             (set! fv1 2))
                      (set! fv0 (+ fv0 fv1))
                      (halt fv0)))])
     (test-match (expose-basic-blocks x)
                  `(module
                     (define ,L.main.1
                       (begin
                         (set! fv0 1)
                         (set! fv1 2)
                         (set! fv0 (+ fv0 fv1))
                         (halt fv0)))))
     (test-ebb-correct x)

     (test-from expose-basic-blocks passes x 3))

   (let ([x `(module
               (begin (begin (set! fv0 1)
                             (set! fv1 2))
                      (begin (begin (set! fv0 (+ fv0 fv1))))
                      (halt fv0)))])
     (test-match (expose-basic-blocks x)
                  `(module
                     (define ,L.main.1
                       (begin
                         (set! fv0 1)
                         (set! fv1 2)
                         (set! fv0 (+ fv0 fv1))
                         (halt fv0)))))

     (test-ebb-correct x)

     (test-from expose-basic-blocks passes x 3))


   (test-suite
    "Something with if and multiple halts"
    (let ([x '(module (begin (set! rbx 1)
                             (set! rcx 2)
                             (if (< rbx rcx)
                                 (begin (set! rdx 4)
                                        (halt rdx))
                                 (begin (set! rdx 8)
                                        (halt rdx)))))])
      (test-match
       (expose-basic-blocks x)
       `(module
          (define ,l2
            (begin (set! rbx 1)
                   (set! rcx 2)
                   (if (< rbx rcx)
                       (jump ,l3)
                       (jump ,l4))))
          (define ,l3
            (begin (set! rdx 4)
                   (halt rdx)))
          (define ,l4
            (begin (set! rdx 8)
                   (halt rdx)))))

      (test-ebb-correct x)))))

(define (a4-public-test-suite
         passes
         link-paren-x64
         interp-paren-x64
         interp-values-lang

         uniquify
         sequentialize-let
         normalize-bind
         select-instructions
         uncover-locals
         undead-analysis
         conflict-analysis
         assign-registers
         replace-locations
         assign-homes-opt
         optimize-predicates
         expose-basic-blocks
         resolve-predicates
         flatten-program
         patch-instructions
         implement-fvars
         generate-x64
         wrap-x64-run-time
         wrap-x64-boilerplate)

  (define decoder (current-actual-decoder))
  (define masker (current-expected-masker))
  (define encoder (current-input-encoder))
  (define run/read (current-run/read))

  (test-suite
   "a4 public test suite"
   #:before
   (thunk
    (current-run/read nasm-run/print-number))
   #:after
   (thunk
    (current-run/read run/read))

   (a4-link-paren-x64-test-suite link-paren-x64)
   (a4-interp-paren-x64-test-suite interp-paren-x64)
   (a4-generate-x64-test-suite passes interp-paren-x64 generate-x64)
   (a4-patch-instructions-test-suite passes patch-instructions)
   (a4-flatten-program-test-suite passes flatten-program)
   (a4-resolve-predicates-test-suite passes resolve-predicates)
   (a4-expose-basic-blocks-test-suite passes expose-basic-blocks)))

;; Local Variables:
;; eval: (put 'module 'racket-indent-function 0)
;; End:
