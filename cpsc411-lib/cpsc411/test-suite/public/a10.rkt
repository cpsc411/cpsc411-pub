#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 (for-syntax
  racket/function
  racket/syntax)
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/performance-hint
 racket/engine
 ;"../../langs/a9.rkt"
 #;"a8.rkt")

(provide (all-defined-out))

(define current-test-timeout (make-parameter 2000))
(define current-omega-timeout (make-parameter 5000))
(define current-omega-stack-limit (make-parameter 16))
(define current-big-fact-stack-limit (make-parameter 32))

(struct exn:fail:timeout exn:fail ())

(define non-zero? (and/c integer? (not/c zero?)))

(define-inline (valid-p expected p . rest)
  `((,expected . (,p ,@rest))
    (,zero? . (,p ,nasm-run/exit-code))))

(define-inline (valid-expr expected expr . rest)
  `((,expected . ((module ,expr) ,@rest))
    (,zero? . ((module ,expr) ,nasm-run/exit-code))))

(define nasm-run/print-string/filter
  (lambda (x)
    (parameterize ([current-error-port
                    (open-output-nowhere)])
      (nasm-run/print-string x))))

(define-inline (invalid-expr code expr)
  `((,code . ((module ,expr) ,nasm-run/exit-code))
    ("" . ((module ,expr) ,nasm-run/print-string/filter))))

(define-inline (make-valid-expr-tests ls)
  (reverse
   (for/fold ([tests '()])
             ([x ls])
     (append (apply valid-expr x) tests))))

(define-inline (make-invalid-expr-tests ls)
  (reverse
   (for/fold ([tests '()])
             ([x ls])
     (append (apply invalid-expr x) tests))))

(define tests
  ;; (listof (name . (listof (flat-contract x execute args))))
  `(
    ("Data types works"
     ,@(make-valid-expr-tests
        `(("#<procedure>" (lambda (x) x) ,nasm-run/print-string)
          ("" (void) ,nasm-run/print-string)
          (42 42)
          (0 0)
          (-42 -42)
          (#t #t)
          (#f #f)
          (() empty)
          (#\a #\a)
          (#\A #\A)
          (,(curry equal? '(1 2)) (cons 1 (cons 2 empty)))
          (,(curry equal? #(1 2 3)) (vector 1 2 3))))
     ,@(invalid-expr 42 '(error 42)))

    ("Predicates work"
     ,@(make-valid-expr-tests
        `((#t (procedure? (lambda (x) x)))
          (#f (procedure? 5))
          (#f (procedure? #t))
          (#f (procedure? #f))
          (#f (procedure? (make-vector 1)))
          (#f (procedure? #\a))
          (#f (procedure? (void)))
          (#f (procedure? empty))
          (#f (procedure? (cons 1 empty)))
          (#f (procedure? (error 42)))

          (#f (fixnum? (lambda (x) x)))
          (#t (fixnum? 5))
          (#f (fixnum? #t))
          (#f (fixnum? #f))
          (#f (fixnum? (make-vector 1)))
          (#f (fixnum? #\a))
          (#f (fixnum? (void)))
          (#f (fixnum? empty))
          (#f (fixnum? (cons 1 empty)))
          (#f (fixnum? (error 42)))

          (#f (boolean? (lambda (x) x)))
          (#f (boolean? 5))
          (#t (boolean? #t))
          (#t (boolean? #f))
          (#f (boolean? (make-vector 1)))
          (#f (boolean? #\a))
          (#f (boolean? (void)))
          (#f (boolean? empty))
          (#f (boolean? (cons 1 empty)))
          (#f (boolean? (error 42)))

          (#f (void? (lambda (x) x)))
          (#f (void? 5))
          (#f (void? #t))
          (#f (void? #f))
          (#f (void? (make-vector 1)))
          (#f (void? #\a))
          (#t (void? (void)))
          (#f (void? empty))
          (#f (void? (cons 1 empty)))
          (#f (void? (error 42)))

          (#f (empty? (lambda (x) x)))
          (#f (empty? 5))
          (#f (empty? #t))
          (#f (empty? #f))
          (#f (empty? (make-vector 1)))
          (#f (empty? #\a))
          (#f (empty? (void)))
          (#t (empty? empty))
          (#f (empty? (cons 1 empty)))
          (#f (empty? (error 42)))

          (#f (pair? (lambda (x) x)))
          (#f (pair? 5))
          (#f (pair? #t))
          (#f (pair? #f))
          (#f (pair? (make-vector 1)))
          (#f (pair? #\a))
          (#f (pair? (void)))
          (#f (pair? empty))
          (#t (pair? (cons 1 empty)))
          (#f (pair? (error 42)))

          (#f (vector? (lambda (x) x)))
          (#f (vector? 5))
          (#f (vector? #t))
          (#f (vector? #f))
          (#t (vector? (make-vector 1)))
          (#f (vector? #\a))
          (#f (vector? (void)))
          (#f (vector? empty))
          (#f (vector? (cons 1 empty)))
          (#f (vector? (error 42)))

          (#f (error? (lambda (x) x)))
          (#f (error? 5))
          (#f (error? #t))
          (#f (error? #f))
          (#f (error? (make-vector 1)))
          (#f (error? #\a))
          (#f (error? (void)))
          (#f (error? empty))
          (#f (error? (cons 1 empty)))
          (#t (error? (error 42)))

          (#f (ascii-char? (lambda (x) x)))
          (#f (ascii-char? 5))
          (#f (ascii-char? #t)) ;; TODO this one?
          (#f (ascii-char? #f))
          (#f (ascii-char? (make-vector 1)))
          (#t (ascii-char? #\a))
          (#f (ascii-char? (void)))
          (#f (ascii-char? empty))
          (#f (ascii-char? (cons 1 empty)))
          (#f (ascii-char? (error 42))))))

    ("Primops work"
     ,@(make-valid-expr-tests
        `((#t (not #f))
          (#f (not #t))
          (#f (not 5))
          (#f (not (error 42)))
          (#f (not (lambda (x) x)))
          (#f (not (void)))
          (#f (not empty))
          (#f (not (cons 1 empty)))
          (#f (not #\a))
          (#f (not (make-vector 1)))

          (#f (not (not #f)))
          (#t (not (not #t)))
          (#t (not (not 5)))
          (#t (not (not (error 42))))
          (#t (not (not (lambda (x) x))))
          (#t (not (not (void))))
          (#t (not (not empty)))
          (#t (not (not (cons 1 empty))))
          (#t (not (not #\a)))
          (#t (not (not (make-vector 1))))

          (#f (< -1 -2))
          (#t (< -2 -1))
          (#t (< 4 5))
          (#f (< 5 4))
          (#f (< 5 5))

          (#f (<= -1 -2))
          (#t (<= -2 -1))
          (#t (<= 4 5))
          (#f (<= 5 4))
          (#t (<= 5 5))

          (#t (> -1 -2))
          (#f (> -2 -1))
          (#f (> 4 5))
          (#t (> 5 4))
          (#f (> 5 5))

          (#t (>= -1 -2))
          (#f (>= -2 -1))
          (#f (>= 4 5))
          (#t (>= 5 4))
          (#t (>= 5 5))

          (#f (eq? -1 -2))
          (#f (eq? -2 -1))
          (#f (eq? 4 5))
          (#f (eq? 5 4))
          (#t (eq? 5 5))
          (#f (eq? #t 5))
          (#f (eq? 5 #t))
          (#f (eq? (lambda (x) x) (make-vector 1)))
          (#f (eq? (make-vector 1) (make-vector 1)))
          (#t (eq? empty empty))
          (#t (eq? (void) (void)))
          (#f (eq? (cons 1 empty) (cons 1 empty)))
          (#t (let ([x (cons 1 empty)]) (eq? x x)))
          (#t (let ([x (make-vector 1)]) (eq? x x)))
          (#t (let ([x (lambda (x) x)]) (eq? x x)))

          (25 (* 5 5))
          (5 (* 1 5))
          (1 (* 1 1))
          (0 (* 0 1))
          (-1 (* -1 1))
          (-42 (* -1 42))
          (0 (* -1 0))

          (10 (+ 5 5))
          (6 (+ 1 5))
          (2 (+ 1 1))
          (1 (+ 0 1))
          (0 (+ -1 1))
          (41 (+ -1 42))
          (-1 (+ -1 0))

          (0 (- 5 5))
          (-4 (- 1 5))
          (0 (- 1 1))
          (-1 (- 0 1))
          (-2 (- -1 1))
          (-43 (- -1 42))
          (-1 (- -1 0))

          (1 (car (cons 1 2)))
          (2 (cdr (cons 1 2)))

          (1 (vector-ref (vector 1) 0))
          (2 (vector-ref (vector 1 2) 1))
          (3 (vector-ref (vector 1 2 3) 2))

          ("" (vector-set! (vector 1) 0 2) ,nasm-run/print-string)
          ("" (vector-set! (vector 1 2) 1 3) ,nasm-run/print-string)
          (#t (void? (vector-set! (vector 1) 0 2)))
          (#t (void? (vector-set! (vector 1 2) 1 3)))
          (,(curry equal? #(2 3 4))
           (let ([p (vector 1 2 3)])
               (begin
                 (vector-set! p 0 2)
                 (vector-set! p 1 3)
                 (vector-set! p 2 4)
                 p)))

          (1 (vector-length (vector 1)))
          (2 (vector-length (vector 1 2)))
          (3 (vector-length (vector 1 2 3)))

          (1 (procedure-arity (lambda (x) x)))
          (2 (procedure-arity (lambda (x y) x)))
          (3 (procedure-arity (lambda (x y z) x)))
          (1 (procedure-arity (let ([y 5]) (lambda (x) y))))))

     ,@(make-invalid-expr-tests
        `((,non-zero? (< #t 5))
          (,non-zero? (< 5 #t))
          (,non-zero? (< (lambda (x) x) (make-vector 1)))

          (,non-zero? (<= #t 5))
          (,non-zero? (<= 5 #t))
          (,non-zero? (<= (lambda (x) x) (make-vector 1)))

          (,non-zero? (> #t 5))
          (,non-zero? (> 5 #t))
          (,non-zero? (> (lambda (x) x) (make-vector 1)))

          (,non-zero? (>= #t 5))
          (,non-zero? (>= 5 #t))
          (,non-zero? (>= (lambda (x) x) (make-vector 1)))

          (,non-zero? (* #t 1))
          (,non-zero? (* 1 #t))

          (,non-zero? (+ #t 1))
          (,non-zero? (+ 1 #t))

          (,non-zero? (- #t 1))
          (,non-zero? (- 1 #t))

          (,non-zero? (car 1))
          (,non-zero? (car '()))
          (,non-zero? (car (void)))
          (,non-zero? (car (lambda (x) x)))
          (,non-zero? (car #f))
          (,non-zero? (car #t))
          (,non-zero? (car #\a))
          (,non-zero? (car (error 42)))
          (,non-zero? (car (vector 1)))

          (,non-zero? (cdr 1))
          (,non-zero? (cdr '()))
          (,non-zero? (cdr (void)))
          (,non-zero? (cdr (lambda (x) x)))
          (,non-zero? (cdr #f))
          (,non-zero? (cdr #t))
          (,non-zero? (cdr #\a))
          (,non-zero? (cdr (error 42)))
          (,non-zero? (cdr (vector 1)))

          (,non-zero? (vector-length 1))
          (,non-zero? (vector-length '()))
          (,non-zero? (vector-length (void)))
          (,non-zero? (vector-length (lambda (x) x)))
          (,non-zero? (vector-length #f))
          (,non-zero? (vector-length #t))
          (,non-zero? (vector-length #\a))
          (,non-zero? (vector-length (error 42)))
          (,non-zero? (vector-length (cons 1 empty)))

          (,non-zero? (vector-ref 1 0))
          (,non-zero? (vector-ref '() 0))
          (,non-zero? (vector-ref (void) 0))
          (,non-zero? (vector-ref (lambda (x) x) 0))
          (,non-zero? (vector-ref #f 0))
          (,non-zero? (vector-ref #t 0))
          (,non-zero? (vector-ref #\a 0))
          (,non-zero? (vector-ref (error 42) 0))
          (,non-zero? (vector-ref (cons 1 empty) 0))
          (,non-zero? (vector-ref (vector 1) #t))
          (,non-zero? (vector-ref (vector 1) #f))
          (,non-zero? (vector-ref (vector 1) '()))
          (,non-zero? (vector-ref (vector 1) (void)))
          (,non-zero? (vector-ref (vector 1) (lambda (x) x)))
          (,non-zero? (vector-ref (vector 1) #\a))
          (,non-zero? (vector-ref (vector 1) (error 42)))
          (,non-zero? (vector-ref (vector 1) (cons 1 empty)))
          (,non-zero? (vector-ref (vector 1) 1))
          (,non-zero? (vector-ref (vector 1) 2))

          (,non-zero? (vector-set! 1 0 1))
          (,non-zero? (vector-set! '() 0 1))
          (,non-zero? (vector-set! (void) 0 1))
          (,non-zero? (vector-set! (lambda (x) x) 0 1))
          (,non-zero? (vector-set! #f 0 1))
          (,non-zero? (vector-set! #t 0 1))
          (,non-zero? (vector-set! #\a 0 1))
          (,non-zero? (vector-set! (error 42) 0 1))
          (,non-zero? (vector-set! (cons 1 empty) 0 1))
          (,non-zero? (vector-set! (vector 1) #t 1))
          (,non-zero? (vector-set! (vector 1) #f 1))
          (,non-zero? (vector-set! (vector 1) '() 1))
          (,non-zero? (vector-set! (vector 1) (void) 1))
          (,non-zero? (vector-set! (vector 1) (lambda (x) x) 1))
          (,non-zero? (vector-set! (vector 1) #\a 1))
          (,non-zero? (vector-set! (vector 1) (error 42) 1))
          (,non-zero? (vector-set! (vector 1) (cons 1 empty) 1))
          (,non-zero? (vector-set! (vector 1) 1 1))
          (,non-zero? (vector-set! (vector 1) 2 1))))

     ,@(for/fold ([tests '()])
                 ([bad-value
                   '(5 #t #f #\a (void) (cons 1 empty) empty (make-vector 1)
                       (error 32))])
         (append (invalid-expr non-zero? `(procedure-arity ,bad-value))
                 tests)))

     ("Primops are Procedures"
      ,@(apply
         append
         (for/list ([proc-x-arity
                     `(,@(map (curry cons 2) '(+ * - eq? cons >= > < <= vector-ref))
                       ,@(map (curry cons 1)
                              '(fixnum? boolean? empty? void? ascii-char? error?
                                        not pair? procedure? vector? car cdr
                                        make-vector vector-length
                                        procedure-arity))
                       (3 . vector-set!))])
           (append
            (valid-expr #t `(procedure? ,(cdr proc-x-arity)))
            (valid-expr (car proc-x-arity) `(procedure-arity ,(cdr proc-x-arity))))))

      ,@(make-invalid-expr-tests
         `((,non-zero? (cons 1))
           (,non-zero? (cons))
           (,non-zero? (cons 1 2 3))

           (,non-zero? (car 1 2))
           (,non-zero? (car '() 2))

           (,non-zero? (cdr 1 2))
           (,non-zero? (cdr '() 2))

           (,non-zero? (vector-ref))
           (,non-zero? (vector-ref 1 2 3))

           (,non-zero? (vector-set!))
           (,non-zero? (vector-set! 1 2 3 4 5 6))

           (,non-zero? (make-vector))
           (,non-zero? (make-vector 1 2 3 4))

           (,non-zero? (procedure-arity))
           (,non-zero? (procedure-arity (lambda (x) x) 2 3 4))

           (,non-zero? (empty?) )
           (,non-zero? (empty? '() '() '()) )

           (,non-zero? (void?))
           (,non-zero? (void? (void) (void) (void)))

           (,non-zero? (pair?))
           (,non-zero? (pair? '() '() '()))

           (,non-zero? (fixnum?))
           (,non-zero? (fixnum? 5 5 5 5))

           (,non-zero? (ascii-char?))
           (,non-zero? (ascii-char? #\a 5 5 5))

           (,non-zero? (boolean?))
           (,non-zero? (boolean? #t #t #t)))))

     ("Overflow Tests"
      ,@(make-valid-expr-tests
         `((#t (fixnum? ,(max-int 61)))
           (#t (fixnum? ,(min-int 61)))

           (,(max-int 61) ,(max-int 61))
           (,(min-int 61) ,(min-int 61))

           (,(min-int 61) (+ 1 ,(max-int 61)))
           (,(max-int 61) (- ,(min-int 61) 1)))))

     ("Binding Tests"
      ,@(valid-p
         #f
         `(module
            (define odd?
              (lambda (x)
                (if (eq? x 0)
                    #f
                    (let ([y (+ x -1)])
                      (even? y)))))
            (define even?
              (lambda (x)
                (if (eq? x 0)
                    #t
                    (let ([y (+ x -1)])
                      (odd? y)))))
            (even? 5)))

      ,@(make-valid-expr-tests
         `((42 (let ([lambda 42]) lambda))
           (42 (let ([define 42]) define))
           (42 (let ([module 42]) module))
           (42 (let ([let 42]) let))
           ("#<procedure>" ((lambda (lambda) (lambda lambda)) (lambda (lambda) lambda))
                           ,nasm-run/print-string)))

      ,@(apply
         append
         (for/list ([bind '(call
                            ; void error ;; TODO these aren't handled right
                            ; because applify needs to come after uniquify
                            * + - eq? < <= >
                            >= fixnum? boolean? empty? void? ascii-char? error? not
                            procedure? vector? pair? cons car cdr make-vector
                            vector-length vector-set! vector-ref
                            procedure-arity)])
           (valid-p
            42
            `(module
               (define ,bind (lambda (,bind) (let ([,bind ,bind]) ,bind)))
               (,bind 42))))))

     ("Stack/Calling Convention Tests"
      ,@(valid-p
         2
         `(module
            (define swap
              (lambda (x y)
                (if (< y x)
                    x
                    (swap y x))))
            (swap 1 2)))

      ,@(valid-p
         (apply + (build-list 16 values))
         `(module
            (define F
              (lambda (a b c d e f g)
                (+ 14 (G a b c d e f g 7))))
            (define G
              (lambda (a b c d e f g h)
                (+ 13 (H a b c d e f g h 8))))
            (define H
              (lambda (a b c d e f g h j)
                (+ 12 (J a b c d e f g h j 9))))
            (define J
              (lambda (a b c d e f g h j k)
                (+ 11 (K a b c d e f g h j k 10))))
            (define K
              (lambda (a b c d e f g h j k l)
                (+ a (+ b (+ c (+ d (+ e (+ f (+ g (+ h (+ j (+ k l))))))))))))
            (+ 15 (F 0 1 2 3 4 5 6))))

      ,@(valid-p
         (apply + (build-list 16 values))
         `(module
            (define F
              (lambda (who a b c d e f g)
                (+ 14 ((car who) (cdr who) a b c d e f g 7))))
            (define G
              (lambda (who a b c d e f g h)
                (+ 13 ((car who) (cdr who) a b c d e f g h 8))))
            (define H
              (lambda (who a b c d e f g h j)
                (+ 12 ((car who) (cdr who) a b c d e f g h j 9))))
            (define J
              (lambda (who a b c d e f g h j k)
                (+ 11 ((car who) a b c d e f g h j k 10))))
            (define K
              (lambda (a b c d e f g h j k l)
                (+ a (+ b (+ c (+ d (+ e (+ f (+ g (+ h (+ j (+ k l))))))))))))

            (let ([who (cons F (cons G (cons H (cons J (cons K '())))))])
              (+ 15 ((car who) (cdr who) 0 1 2 3 4 5 6))))))

     ("Closure Tests"
      ,@(make-valid-expr-tests
         `((5 (let ([x 5]) ((lambda (y) x) 2)))
           (,(+ 5 6 7)
            (let ([x 5] [y 6] [z 7])
              ((lambda (a) (+ (+ x y) z)) 2)))))
      ,@(valid-p
         (curry equal? `((1 1 1 1 1)
                         .
                         (6 5 4 3 2)))
         `(module
            (define* (zero? n)
              (eq? n 0))

            (define* (sub1 n)
              (- n 1))

            (define* (curry f x)
              (lambda (y)
                (f x y)))

            (define* (build-list f n)
              (if (zero? n)
                  '()
                  (cons (f n) (build-list f (sub1 n)))))

            (cons
             (build-list (lambda (n) 1) 5)
             (build-list (curry + 1) 5)))))

     ("Big Tests"
      ,@(valid-expr
         (curry equal? '#(#\h #\e #\l #\l #\o #\space #\w #\o #\r #\l #\d))
         '(vector #\h #\e #\l #\l #\o #\space #\w #\o #\r #\l #\d))

      ,@(valid-p
         (lambda (x) (apply <= x))
         '(module
           (define* (filter f ls)
             (if (empty? ls)
                 ls
                 (let ([x (car ls)])
                   ((lambda (y)
                      (if (f x)
                          (cons x y)
                          y))
                    (filter f (cdr ls))))))

           (define* (append ls1 ls2)
             (if (empty? ls1)
                 ls2
                 (cons (car ls1) (append (cdr ls1) ls2))))

           (define* (quicksort ls)
             (cond
               [(or (empty? ls)
                    (empty? (cdr ls)))
                ls]
               [else
                (let ([pivot (car ls)]
                      [rst (cdr ls)])
                  (append
                   (append
                    (quicksort
                     (filter (lambda (x) (< x pivot)) rst))
                    (cons pivot empty))
                   (quicksort
                    (filter (lambda (x) (>= x pivot)) rst))))]))

           (define* (mod x y)
             (if (>= x y)
                 (mod (- x y) y)
                 x))

           (define* (build-list f len)
             (if (eq? len 0)
                 empty
                 (cons (f len) (build-list f (- len 1)))))

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
             (quicksort (build-list (lambda (x) (random)) 10000)))))

      ,@(valid-p
         (curry
          equal?
          '(1
            #(#\h #\a #\v #\e #\a #\b #\a #\n #\a #\n #\a)
            #(#\f #\i #\z #\z)
            #(#\h #\a #\v #\e #\a #\b #\a #\n #\a #\n #\a)
            #(#\b #\u #\z #\z)
            #(#\h #\a #\v #\e #\a #\b #\a #\n #\a #\n #\a)
            7
            #(#\h #\a #\v #\e #\a #\b #\a #\n #\a #\n #\a)
            #(#\f #\i #\z #\z)
            #(#\h #\a #\v #\e #\a #\b #\a #\n #\a #\n #\a)))
         `(module
            (define* (zero? n)
              (eq? n 0))

            (define* (mod x y)
              (if (>= x y)
                  (mod (- x y) y)
                  x))

            (define* (sub1 n) (- n 1))

            (define* (append ls1 ls2)
              (if (empty? ls1)
                  ls2
                  (cons (car ls1) (append (cdr ls1) ls2))))

            (define* (reverse ls)
              (if (empty? ls)
                  '()
                  (append (reverse (cdr ls)) (cons (car ls) '()))))

            (define* (fizzbuzz n)
              (cond
                [(zero? n)
                 '()]
                [(zero? (mod n 2))
                 ;; The Arthur Shappey version
                 (cons (vector #\h #\a #\v #\e #\a #\b #\a #\n #\a #\n #\a) (fizzbuzz (sub1 n)))]
                [(and (zero? (mod n 3)) (zero? (mod n 5)))
                 (cons (vector #\f #\i #\z #\z #\b #\u #\z #\z) (fizzbuzz (sub1 n)))]
                [(zero? (mod n 3))
                 (cons (vector #\f #\i #\z #\z) (fizzbuzz (sub1 n)))]
                [(zero? (mod n 5))
                 (cons (vector #\b #\u #\z #\z) (fizzbuzz (sub1 n)))]
                [else (cons n (fizzbuzz (sub1 n)))]))

            (let ([fizzbuzz (lambda (x) (reverse (fizzbuzz x)))]) (fizzbuzz 10)))))

     ))

(define (a10-public-test-suite)

  (define decoder (current-actual-decoder))
  (define masker (current-expected-masker))
  (define encoder (current-input-encoder))
  (define run/read (current-run/read))

  (make-test-suite
   "a11 public test suite"

   (append
    (for/list ([suite tests])
      (make-test-suite
       (car suite)
       (for/list ([case (cdr suite)]
                  [i (in-naturals 1)])
         (test-suite
          (format "Test ~a" i)
          (with-check-info (['expected (car case)]
                            ['actual-expr (cdr case)])
            (test-begin
              (let ([e (engine (lambda (_) (apply execute (cdr case))))])
                (if (engine-run (current-test-timeout) e)
                    (with-check-info (['actual (engine-result e)])
                      (check-pred (flat-contract-predicate (car case)) (engine-result e)))
                    (with-check-info (['engine-result (engine-result e)])
                      (fail "Test timed out"))
                    ;; TODO: Test suites broken
                    #;(raise (exn:fail:timeout "Test timed out"
                                               (current-continuation-marks)))))))))))

    (list
     (test-suite
      "a11 tail calls"
      (test-case "Omega"
        (let ([stack-limit (current-omega-stack-limit)]
              [str (compile '(module ((lambda (x) (x x))
                                      (lambda (x) (x x)))))])
          (let-values ([(out in pid err control-f)
                        (apply
                         values
                         ((nasm-run/observe
                           #:block? #f
                           (lambda (x)
                             (process (format "ulimit -Ss ~a -Hs ~a; ~a" stack-limit stack-limit x))))
                          str))])
            (let ([e (engine (lambda (_) (control-f 'wait) (control-f 'exit-code)))])
              (if (engine-run (current-omega-timeout) e)
                  (if (eq? 0 (engine-result e))
                      (fail "Omega terminated normally")
                      (with-check-info (['ulimit-exit (engine-result e)])
                        (fail "Omega ate too much memory")))
                  (control-f 'kill))))))

      (test-case "Big Fact"
        (let ([stack-limit (current-big-fact-stack-limit)]
              [str (compile '(module
                                 (define* (fact n acc)
                                   (if (eq? n 0)
                                       acc
                                       (fact (- n 1) (* n acc))))
                                 (fact 19 1)))])
          (check-equal?
           ((nasm-run/observe (lambda (x)
                                (with-input-from-string
                                  (with-output-to-string
                                    (thunk
                                     (let ([exit-code
                                            (system/exit-code (format "ulimit -Ss ~a -Hs ~a; ~a"
                                                                      stack-limit
                                                                      stack-limit
                                                                      x))])
                                       (if (zero? exit-code)
                                           (void)
                                           (with-check-info (['ulimit-exit exit-code])
                                             (fail "Fact ate too much memory"))))))
                                  read))) str)

           121645100408832000))))))

   #:before
   (thunk
    (current-run/read nasm-run/read))

   #:after
   (thunk
    (current-run/read run/read))))
