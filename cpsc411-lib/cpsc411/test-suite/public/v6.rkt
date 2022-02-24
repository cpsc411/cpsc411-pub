#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib
 racket/dict
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
 "v5.rkt")

(provide (all-defined-out))

(for ([(new-v alias-v)
       (in-dict
        `(
          (,interp-values-lang-v6 . ,interp-values-lang-v5)
          (,interp-values-unique-lang-v6 . ,interp-values-unique-lang-v5)
          (,interp-imp-mf-lang-v6 . ,interp-imp-mf-lang-v5)
          (,interp-proc-imp-cmf-lang-v6 . ,interp-proc-imp-cmf-lang-v5)

          ;; TODO: backwards compatibility issues with reordering implement-frame-vars
          #;(,interp-nested-asm-lang-v6 . ,interp-nested-asm-lang-v5)
          (,interp-paren-x64-v6 . ,interp-paren-x64-v5)))])
  (register-test-programs!
   new-v
   (set->list (hash-ref test-prog-dict alias-v '()))))

(define (replace-all f? k tree)
  (match tree
    ['() '()]
    [(? f?)
     (k tree)]
    [(list trees ...)
     (map (curry replace-all f? k) trees)]
    [_ tree]))


(define (replace-halts p [done 'r15])
  (replace-all
   (lambda (x) (match x [`(halt ,v) #t] [_ #f]))
   (lambda (x)
     (match x
       [`(halt ,v)
        `(begin
           (set! rax ,v)
           (jump ,done))]))
   p))

(define (replace-implicit-return p)
  (define (replace-implicit-return-tail p)
    (match p
      [`(begin ,effect ... ,tail)
       `(begin ,@effect ,(replace-implicit-return-tail tail))]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,pred
            ,(replace-implicit-return-tail tail1)
            ,(replace-implicit-return-tail tail2))]
      [`(jump ,trg ,loc ...)
       p]
      [_ `(begin (set! rax ,p) (jump r15))]))
  (match p
    [`(module (define ,labels ,tails) ... ,tail)
     `(module ,@(for/list ([label labels]
                           [tail tails])
                  `(define ,label ,(replace-implicit-return-tail tail)))
              ,(replace-implicit-return-tail tail))]))

(define (add-new-frames p)
  (define (add-frames-blocks def)
    (match def
      [`(define ,label ,info ,tail)
       `(define ,label ,(info-set info 'new-frames '(())) ,tail)]))
  (match p
    [`(module ,info ,defs ... ,tail)
     `(module ,(info-set info 'new-frames '(()))
              ,@(map add-frames-blocks defs)
              ,tail)]))

;; TODO compatibility problem with implicit vs explicit return
(register-test-programs!
 interp-imp-cmf-lang-v6
 (filter (lambda (x) (imp-cmf-lang-v6? (second x)))
         (map (lambda (x)
                (list (first x) (replace-implicit-return (second x))))
              (set->list (hash-ref test-prog-dict interp-imp-cmf-lang-v5 '())))))

(for ([(new-v alias-v)
       (in-dict
        `(;; TODO: backwards compatibility issues with halt
          (,interp-asm-pred-lang-v6 . ,interp-asm-pred-lang-v5)
          (,interp-asm-pred-lang-v6/locals . ,interp-asm-pred-lang-v5/locals)
          ;; patch can't update undead-out sets
          #;(,interp-asm-pred-lang-v6/undead . ,interp-asm-pred-lang-v5/undead)
          (,interp-asm-pred-lang-v6/conflicts . ,interp-asm-pred-lang-v5/conflicts)
          (,interp-asm-pred-lang-v6/assignments . ,interp-asm-pred-lang-v5/assignments)
          ))])
  (register-test-programs!
   new-v
   (map (lambda (x)
          (list (first x) (add-new-frames (replace-halts (second x)))))
    (set->list (hash-ref test-prog-dict alias-v '())))))

(define (fixup-begin p)
  (define (fixup-i i)
    (match i
      [`(begin ,is ...)
       (append-map fixup-i is)]
      [`(with-label ,label ,i)
       (match (fixup-i i)
         [(cons i is)
          (cons `(with-label ,label ,i) is)])]
      [_ (list i)]))
  (match p
    [`(begin ,is ...)
     `(begin
        ,@(for/fold ([r '()])
                    ([i is])
            (append r (fixup-i i))))]))

(define (replace-fvars x)
  (replace-all
   fvar?
   (lambda (x)
     `[,(current-frame-base-pointer-register) - ,(* 8 (fvar->index x))])
   x))

(register-test-programs!
 interp-para-asm-lang-v6
 (map (lambda (x)
        (list (first x) (replace-fvars (fixup-begin (replace-halts (second x) 'done)))))
      (set->list (hash-ref test-prog-dict interp-para-asm-lang-v5 '()))))

(register-test-programs!
 interp-asm-pred-lang-v6/undead
 '((""
    (module
      ((locals (x.1 y.2))
       (call-undead ())
       (new-frames (()))
       (undead-out
        ((x.1) (x.1 y.2) ((x.1 y.2) (() ()) (() ())))))
      (begin (set! x.1 3) (set! y.2 x.1) (if (> y.2 x.1)
                                             (begin (set! rax x.1)
                                                    (jump r15))
                                             (begin (set! rax y.2)
                                                    (jump r15))))))))

(register-test-programs!
 interp-values-lang-v6
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
          (if (= x 0)
              1
              (let ([z (+ x -1)])
                (let ([y (call fact z)])
                  (* x y))))))
      (call fact 5)))

   (""
    (module
      (define fact
        (lambda (x)
          (if (= x 0)
              1
              (let ([z (+ x -1)])
                (let ([y (call fact z)])
                  (* x y))))))
      (call fact 10)))

   (""
    (module
      (define identity
        (lambda (x)
          (if (= x 0)
              0
              (let ([y (- x 1)])
                (let ([x (call identity y)])
                  (+ 1 x))))))

      (define fact
        (lambda (x)
          (let ([x (call identity x)]
                [y (call identity 0)])
            (if (= x y)
                (let ([z (call identity 1)])
                  z)
                (let ([n (call identity 1)])
                  (let ([z (- x n)])
                    (let ([y (call fact z)])
                      (* x y))))))))

      (call fact 5)))))

(define (v6-public-test-suite pass-ls interp-ls)
  (define run/read (current-run/read))
  (define passes (current-pass-list))

  (test-suite
   "v6 public test suite"
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
