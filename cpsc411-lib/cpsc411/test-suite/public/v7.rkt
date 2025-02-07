#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib
 racket/dict
 "../../langs/v7.rkt"
 "../../langs/v6.rkt"
 "../utils.rkt"
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

(define (replace-int64-exprs-to-ptr prog)
  (replace-all integer? i64->ptr prog))

(for ([(new-v alias-v)
       (in-dict
        `(
          (,interp-exprs-bits-lang-v7 . ,interp-values-unique-lang-v6)
          (,interp-values-bits-lang-v7 . ,interp-values-unique-lang-v6)
          (,interp-imp-mf-lang-v7 . ,interp-imp-mf-lang-v6)
          (,interp-proc-imp-cmf-lang-v7 . ,interp-proc-imp-cmf-lang-v6)))])
  (register-test-programs!
   new-v
   (for/list ([test-entry
               (set->list (hash-ref test-prog-dict alias-v '()))])
     (list (first test-entry)
           (replace-int64-exprs-to-ptr (second test-entry))))))

(define (opand->ptr v)
  (if (integer? v)
      (i64->ptr v)
      v))

(define (replace-int-opands-to-ptr p)
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
       [_ x])) p))

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
   (for/list ([test-entry
               (set->list (hash-ref test-prog-dict alias-v '()))])
     (list (first test-entry)
           (replace-int-opands-to-ptr (second test-entry))))))

(define (i32-opand->ptr v)
  (if (integer? v)
      (i32->ptr v)
      v))

(register-test-programs!
 interp-paren-x64-v7
 (for/list ([test-entry
             (set->list (hash-ref test-prog-dict interp-paren-x64-v6 '()))]
            ;; A stupid way to detect multiplication
            #:when (equal? (replace-all
                            (lambda (x)
                              (eq? x '*))
                            (lambda (x)
                              #f)
                            test-entry)
                           test-entry))
   (list
    (first test-entry)
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
     (second test-entry)))))

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
    (module #t))

   (""
    (module #f))

   (""
    (module empty))
   ))

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
