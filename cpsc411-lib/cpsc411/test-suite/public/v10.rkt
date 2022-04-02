#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib
 racket/dict
 "../../langs/v9.rkt"
 "../../langs/v11.rkt"
 "../utils.rkt"
 "v9.rkt")

(provide (all-defined-out))

(for ([(new-v alias-v)
       (in-dict
        `(
          (,interp-racketish-surface . ,interp-exprs-lang-v9)))])
  (register-test-programs! new-v (set->list (hash-ref test-prog-dict alias-v '()))))

(register-test-programs!
 interp-racketish-surface
 `((""
    (module (and #f (error 5))))

   (""
    (module (or #t (error 5))))

   (""
    (module (and #f 42)))

   (""
    (module (or #f 42)))

   (""
    (module (quote (1 2 3))))

   (""
    (module (quote ())))

   (""
    (module (vector 1 2 3)))

   (""
    (module (vector (+ 1 2) 3 2 3)))

   (""
    (module (begin 1 2)))

   (""
    (module ((lambda (x) x) 1)))

   (""
    (module
      (vector #\h #\e #\l #\l #\o #\space #\w #\o #\r #\l #\d)))

   #;(""
    (module
      (define filter
        (lambda (f ls)
          (if (empty? ls)
              ls
              (let ([x (car ls)])
                ((lambda (y)
                   (if (f x)
                       (cons x y)
                       y))
                 (filter f (cdr ls)))))))

      (define append
        (lambda (ls1 ls2)
          (if (empty? ls1)
              ls2
              (cons (car ls1) (append (cdr ls1) ls2)))))

      (define quicksort
        (lambda (ls)
          (if (or (empty? ls)
                  (empty? (cdr ls)))
              ls
              (let ([pivot (car ls)]
                    [rst (cdr ls)])
                (append
                 (append
                  (quicksort
                   (filter (lambda (x) (< x pivot)) rst))
                  (cons pivot empty))
                 (quicksort
                  (filter (lambda (x) (>= x pivot)) rst)))))))

      (define mod
        (lambda (x y)
          (if (>= x y)
              (mod (- x y) y)
              x)))

     (define build-list
       (lambda (f len)
         (if (eq? len 0)
             empty
             (cons (f len) (build-list f (- len 1))))))

     (define sorted?
       (lambda (ls)
         (if (eq? empty ls)
             #t
             (if (eq? empty (cdr ls))
                 #t
                 (and (< (car ls) (car (cdr ls)))
                      (sorted? (cdr (cdr ls))))))))

     (let* ([x (make-vector 1)]
            [random (lambda ()
                      (let ([A 12312]
                            [C 1]
                            [MAX 100])
                        (let ([p (mod
                                  (+ C (* A (vector-ref x 0)))
                                  MAX)])
                          (begin
                            (vector-set! x 0 p)
                            p))))])
       (sorted? (quicksort (build-list (lambda (x) (random)) 20))))))))

(define (411-quicksort)
  '(module
     (define filter
       (lambda (f ls)
         (if (empty? ls)
             ls
             (let ([x (car ls)])
               ((lambda (y)
                  (if (f x)
                      (cons x y)
                      y))
                (filter f (cdr ls)))))))

     (define append
       (lambda (ls1 ls2)
         (if (empty? ls1)
             ls2
             (cons (car ls1) (append (cdr ls1) ls2)))))

     (define quicksort
       (lambda (ls)
         (if (or (empty? ls)
                 (empty? (cdr ls)))
             ls
             (let ([pivot (car ls)]
                   [rst (cdr ls)])
               (append
                (append
                 (quicksort
                  (filter (lambda (x) (< x pivot)) rst))
                 (cons pivot empty))
                (quicksort
                 (filter (lambda (x) (>= x pivot)) rst)))))))

     (define mod
       (lambda (x y)
         (if (>= x y)
             (mod (- x y) y)
             x)))

     (define build-list
       (lambda (f len)
         (if (eq? len 0)
             empty
             (cons (f len) (build-list f (- len 1))))))

     (define sorted?
       (lambda (ls)
         (if (eq? empty ls)
             #t
             (if (eq? empty (cdr ls))
                 #t
                 (and (< (car ls) (car (cdr ls)))
                      (sorted? (cdr (cdr ls))))))))

     (let* ([x (make-vector 1)]
            [random (lambda ()
                      (let ([A 12312]
                            [C 1]
                            [MAX 100])
                        (let ([p (mod
                                  (+ C (* A (vector-ref x 0)))
                                  MAX)])
                          (begin
                            (vector-set! x 0 p)
                            p))))])
       (sorted? (quicksort (build-list (lambda (x) (random)) 20))))))

(define (v10-public-test-suite pass-ls interp-ls)
  (define run/read (current-run/read))
  (define passes (current-pass-list))

  (test-suite
   "v10 public test suite"
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
    (compiler-testomatic pass-ls interp-ls nasm-run/read))

   (test-suite
    "quicksort"
    (execute (411-quicksort)))))
