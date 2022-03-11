#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib
 racket/dict
 "../../langs/v7.rkt"
 "../../langs/v6.rkt"
 "../../langs/v5.rkt"
 "../../langs/v4.rkt"
 "../../langs/v3.rkt"
 "../../langs/v2.rkt"
 "../utils.rkt"
 "v1.rkt"
 "v2.rkt"
 "v3.rkt"
 "v4.rkt"
 "v5.rkt"
 "v6.rkt")

(provide (all-defined-out))

(define (i64->ptr i)
  (cond
    [(int61? i)
     (* i 8)]
    [(int64? i)
     (* 8 (max-int 61))]))

(define (i32->ptr i)
  (cond
    [(int-size? 29 i)
     (* i 8)]
    [(int32? i)
     (* 8 (max-int 29))]))

(for ([(new-v alias-v)
       (in-dict
        `(
          (,interp-exprs-bits-lang-v7 . ,interp-values-unique-lang-v6)
          (,interp-values-bits-lang-v7 . ,interp-values-unique-lang-v6)
          (,interp-imp-mf-lang-v7 . ,interp-imp-mf-lang-v6)
          (,interp-proc-imp-cmf-lang-v7 . ,interp-proc-imp-cmf-lang-v6)))])
  (register-test-programs!
   new-v
   (for/list ([test-prog
               (set->list (hash-ref test-prog-dict alias-v '()))])
     (replace-all
      integer?
      i64->ptr
      test-prog))))

(define (opand->ptr v)
  (if (integer? v)
      (i64->ptr v)
      v))

(for ([(new-v alias-v)
       (in-dict
        `(
          (,interp-asm-pred-lang-v7 . ,interp-asm-pred-lang-v6)
          (,interp-asm-pred-lang-v7/locals . ,interp-asm-pred-lang-v6/locals)
          (,interp-asm-pred-lang-v7/undead . ,interp-asm-pred-lang-v6/undead)
          (,interp-asm-pred-lang-v7/conflicts . ,interp-asm-pred-lang-v6/conflicts)
          (,interp-asm-pred-lang-v7/assignments . ,interp-asm-pred-lang-v6/assignments)))])
  (register-test-programs!
   new-v
   (for/list ([test-prog
               (set->list (hash-ref test-prog-dict alias-v '()))])
     (replace-all
      (lambda (x)
        (match x
          [`(set! ,x ,v)
           #:when (not (frame-base-pointer-register? x))
           #t]
          [_ #f]))
      (lambda (x)
        (match x
          [`(set! ,x (,binop ,v1 ,v2))
           `(set! ,x (,binop ,(opand->ptr v1) ,(opand->ptr v2)))]
          [`(set! ,x ,v)
           `(set! ,x ,(opand->ptr v))]
          [_ x]))
      test-prog))))

(define (i32-opand->ptr v)
  (if (integer? v)
      (i32->ptr v)
      v))

(register-test-programs!
 interp-paren-x64-v7
 (for/list ([test-prog
             (set->list (hash-ref test-prog-dict interp-paren-x64-v6 '()))]
            ;; A stupid way to detect multiplication
            #:when (equal? (replace-all
                            (lambda (x)
                              (eq? x '*))
                            (lambda (x)
                              #f)
                            test-prog)
                           test-prog))
   (replace-all
    (lambda (x)
      (match x
        [`(set! ,x (,binop ,x ,v))
         #:when (not (frame-base-pointer-register? x))
         #t]
        [`(set! ,x ,v)
         #t]
        [_ #f]))
    (lambda (x)
      (match x
        [`(set! ,x (,binop ,v1 ,v2))
         #:when (memq binop '(+ -))
         `(set! ,x (,binop ,(i32-opand->ptr v1) ,(i32-opand->ptr v2)))]
        [`(set! ,x ,v)
         `(set! ,x ,(opand->ptr v))]
        [_ x]))
    test-prog)))

(register-test-programs!
 interp-exprs-lang-v7
 '((""
    (module
      (define id
        (lambda (x)
          x))
      (let ([x (call id 5)])
        (call id x))))

   (""
    (module
      (define fact
        (lambda (x)
          (if (call eq? x 0)
              1
              (let ([z (call + x -1)])
                (let ([y (call fact z)])
                  (call * x y))))))
      (call fact 5)))

   (""
    (module
      (define fact
        (lambda (x)
          (if (call eq? x 0)
              1
              (let ([z (call + x -1)])
                (let ([y (call fact z)])
                  (call * x y))))))
      (call fact 10)))

   (""
    (module
      (define identity
        (lambda (x)
          (if (call eq? x 0)
              0
              (let ([y (call - x 1)])
                (let ([x (call identity y)])
                  (call + 1 x))))))

      (define fact
        (lambda (x)
          (let ([x (call identity x)]
                [y (call identity 0)])
            (if (call eq? x y)
                (let ([z (call identity 1)])
                  z)
                (let ([n (call identity 1)])
                  (let ([z (call - x n)])
                    (let ([y (call fact z)])
                      (call * x y))))))))

      (call fact 5)))

   (""
    (module (call + (call + 5 6) (call * 4 5))))
   (""
    (module (if (call eq? (call + 5 6) 11) 4 6)))
   (""
    (module #t))
   (""
    (module #f))
   (""
    (module empty))
   #;(module (void))

   ("blerner's stack smasher"
    (module
      (define F
        (lambda (a b c d e f g)
          (call + 10 (call G a b c d e f g 8))))
      (define G
        (lambda (a b c d e f g h)
          (call H a b c d e f g h 9)))
      (define H
        (lambda (a b c d e f g h j)
          (let ([r1 (call + a b)])
            (let ([r2 (call + r1 c)])
              (let ([r3 (call + r2 d)])
                (let ([r4 (call + r3 e)])
                  (let ([r5 (call + r4 f)])
                    (let ([r6 (call + r5 g)])
                      (let ([r7 (call + r6 h)])
                        (call + r7 j))))))))))
      (call F 1 2 3 4 5 6 7)))

   ("swap"
    (module
      (define swap
        (lambda (x y)
          (if (call < y x)
              x
              (call swap y x))))
      (call swap 1 2)))

   ("ae fact tail"
    (module
      (define fact_loop
        (lambda (n acc)
          (if (call eq? n 0)
              acc
              (call fact_loop (call - n 1) (call * acc n)))))
      (call fact_loop 5 1)))

   ("ae fact non-tail"
    (module
      (define fact
        (lambda (x)
          (if (call eq? x 0)
              1
              (call * x (call fact (call - x 1))))))
      (call fact 5)))

   ("values fib tail"
    (module
      (define fib_loop
        (lambda (n acc1 acc2)
          (if (call eq? n 0)
              acc1
              (if (call eq? n 1)
                  acc2
                  (let ([new-n (call + n -1)])
                    (let ([new-acc2 (call + acc1 acc2)])
                      (call fib_loop new-n acc2 new-acc2)))))))
      (call fib_loop 5 0 1)))))

(define (v7-public-test-suite pass-ls interp-ls)
  (define run/read (current-run/read))
  (define passes (current-pass-list))

  (test-suite
   "v7 public test suite"
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
