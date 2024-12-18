#lang reader "../../test-lang/lang/reader.rkt"

(require
 racket/match
 rackunit
 cpsc411/test-suite/utils
 cpsc411/compiler-lib)

(provide (all-defined-out))

; validates register assignments in assign-registers
(define (validate-assignments locals conflicts assigns [regs (current-assignable-registers)])
  (let* ([a-hash (make-hash assigns)]
         [a-values (hash-values a-hash)]
         [f (lambda (e acc) (+ acc (cond [(set-member? a-values (list e)) 1]
                                         [else 0])))])
    (and
     ; all locals are assigned
     (equal? (length assigns) (length locals))
     (foldr (lambda (e acc) (and acc (hash-has-key? a-hash e))) #t locals)
     ; no conflicts
     (foldr
      (lambda (p acc)
        (let* ([v-names (cons (car p) (car (cdr p)))]
               [v-values (foldr (lambda (v acc) (cons (hash-ref a-hash v) acc)) '() v-names)])
          (and acc (equal? (length v-names) (length v-values)))))
      '()
      conflicts)
     ; should not spill early
     ;(begin (println a-values) (println (foldr f 0 regs))
     (<= (foldr f 0 regs) (length locals)))))

; validates conflicts match expected
(define (validate-conflicts locals conflicts expected)
  (let ([c-hash (make-hash conflicts)]
        [e-hash (make-hash expected)])
    (foldr (lambda (e acc) (and acc (set=? (car (hash-ref c-hash e)) (car (hash-ref e-hash e))))) #t locals)))

; validates undead sets
(define (validate-undead undead expected)
  (let ([f (map (lambda (u e) (equal? (list->set u) (list->set e))) undead expected)])
    (foldr (lambda (x acc) (and acc x)) #t f)))

(define (get-undead-info p)
  (info-ref (second p) 'undead-out))

(define-check (check-a3-undead-sets-equal? actuals expecteds)
  (for ([s1 (get-undead-info actuals)]
        [s2 expecteds]
        [i (in-naturals)])
    (unless (set=? s1 s2)
      (fail-check (format "Expected undead set ~a for instruction number ~a, but got ~a" s2 i s1)))))

(define (a3-public-test-undead-sets undead-analysis)
  (test-suite
   "a3 undead-analysis test suite"

   (test-case "Simple case with one variable"
     (with-check-info (['undead-out (string-info "Undead of the form: (undead-out ((x.1) ()))")])
       (check-match
        (undead-analysis
         '(module
            ((locals (x.1)))
            (begin
              (set! x.1 5)
              (halt x.1))))
        `(module
           ,info
           (begin
             (set! x.1 5)
             (halt x.1)))
        (validate-undead (info-ref info 'undead-out) '((x.1) ())))))

   (test-case "Simple case with two variables"
     (let ([e '((x.1) (x.1) ())])
       (with-check-info (['undead-out (string-info (format "Undead of the form: (undead-out ~a)" e))])
         (check-a3-undead-sets-equal?
          (undead-analysis
           '(module
              ((locals (x.1 x.2)))
              (begin
                (set! x.1 5)
                (set! x.2 10)
                (halt x.1))))
          e))))

   (test-case "Simple case with two variables"
     (let ([e '((x.1) (x.1 x.2) (x.2) ())])
       (with-check-info (['undead-out (string-info (format "Undead of the form: (undead-out ~a)" e))])
         (check-a3-undead-sets-equal?
          (undead-analysis
           '(module
              ((locals (x.1 x.2)))
              (begin
                (set! x.1 5)
                (set! x.2 10)
                (set! x.1 (+ x.1 x.2))
                (halt x.2))))
          e))))

   (test-case "Simple case with three variables"
     (let ([e '((x.1) (x.1 x.2) (x.1 x.2 x.3) (x.2) ())])
       (with-check-info (['undead-out (string-info (format "Undead of the form: (undead-out ~a)" e))])
         (check-a3-undead-sets-equal?
          (undead-analysis
           '(module
              ((locals (x.1 x.2 x.3)))
              (begin
                (set! x.1 5)
                (set! x.2 10)
                (set! x.3 x.2)
                (set! x.3 (* x.3 x.1))
                (halt x.2))))
          e))))

   (test-case "Complex case with three variables"
     (let ([e '((x.1) (x.1 x.3) (x.3 x.1) (x.1) ())])
       (with-check-info (['undead-out (string-info (format "Undead of the form: (undead-out ~a)" e))])
         (check-a3-undead-sets-equal?
          (undead-analysis
           '(module
              ((locals (x.1 x.2 x.3)))
              (begin
                (set! x.1 5)
                (set! x.3 15)
                (set! x.2 x.1)
                (set! x.1 (* x.1 x.3))
                (halt x.1))))
          e))))

   (test-case "Complex case with three variables"
     (let ([e '((x.1) (x.1 x.2) (x.2) (x.3 x.2) (x.3 x.1) () (x.3) ())])
       (with-check-info (['undead-out (string-info (format "Undead of the form: (undead-out ~a)" e))])
         (check-a3-undead-sets-equal?
          (undead-analysis
           '(module
              ((locals (x.1 x.2 x.3)))
              (begin
                (set! x.1 5)
                (set! x.2 10)
                (set! x.1 (+ x.1 x.1))
                (set! x.3 15)
                (set! x.1 x.2)
                (set! x.1 (* x.1 x.3))
                (set! x.3 5)
                (halt x.3))))
          e))))

   (test-case "Big Program"
     (let ([e '((v.1) (v.1 w.2) (w.2 x.3) (t.7 w.2 x.3) (w.2 x.3) (y.4 w.2 x.3)
                (t.8 y.4 w.2 x.3) (y.4 w.2 x.3) (z.5 y.4 w.2) (z.5 y.4) (t.6 z.5)
                (t.6 z.5 t.9) (t.6 z.5) (z.5) ())])
       (with-check-info (['undead-out (string-info (format "Undead of the form: (undead-out ~a)" e))])
         (check-a3-undead-sets-equal?
          (undead-analysis
           '(module
              ((locals (v.1 w.2 x.3 y.4 z.5 t.6 t.7 t.8 t.9)))
              (begin
                (set! v.1 1)
                (set! w.2 46)
                (set! x.3 v.1)
                (set! t.7 7)
                (set! x.3 (+ x.3 t.7))
                (set! y.4 x.3)
                (set! t.8 4)
                (set! y.4 (+ y.4 t.8))
                (set! z.5 x.3)
                (set! z.5 (+ z.5 w.2))
                (set! t.6 y.4)
                (set! t.9 -1)
                (set! t.6 (* t.6 t.9))
                (set! z.5 (+ z.5 t.6))
                (halt z.5))))
          e))))))

(define (a3-assign-registers-stress-tests assign-registers)
  (define test-locals1 '(x.1 x.2 x.3))
  (define test-conflicts1 '((x.3 (x.2 x.1)) (x.1 (x.2 x.3)) (x.2 (x.1 x.3))))
  (define test-prog1
    `(module
       ((locals ,test-locals1)
        (conflicts ,test-conflicts1))
       (begin
         (set! x.1 1)
         (set! x.2 2)
         (set! x.3 3)
         (set! x.3 (+ x.3 x.2))
         (set! x.3 (+ x.3 x.1))
         (halt x.3))))

  (define test-locals2 '(x.1 x.2 x.3))
  (define test-conflicts2 '((x.3 (x.1)) (x.1 (x.2 x.3)) (x.2 (x.1))))
  (define test-prog2 `(module
                        ((locals ,test-locals2)
                         (conflicts ,test-conflicts2))
                        (begin
                          (set! x.1 10)
                          (set! x.2 15)
                          (set! x.1 (+ x.1 x.2))
                          (set! x.3 5)
                          (set! x.2 (+ x.2 x.1))
                          (set! x.2 (+ x.2 x.3))
                          (halt x.1))))


  (test-suite
   "a3 assign-registers stress tests"

   (test-suite "Simple large test"
     (let* ([locals '(v.1 w.2 x.3 y.4 z.5 t.6)]
            [conflicts '((x.3 (z.5 y.4 v.1 w.2))
                         (w.2 (z.5 y.4 v.1 x.3))
                         (v.1 (w.2 x.3))
                         (y.4 (t.6 z.5 w.2 x.3))
                         (z.5 (t.6 y.4 w.2 x.3))
                         (t.6 (z.5 y.4)))]
            [p1 `(module
                     ((locals ,locals)
                      (conflicts ,conflicts))
                     (begin
                       (set! v.1 1)
                       (set! w.2 46)
                       (set! x.3 v.1)
                       (set! x.3 (+ x.3 7))
                       (set! y.4 x.3)
                       (set! y.4 (+ y.4 4))
                       (set! z.5 x.3)
                       (set! z.5 (+ z.5 w.2))
                       (set! t.6 y.4)
                       (set! t.6 (* t.6 -1))
                       (set! z.5 (+ z.5 t.6))
                       (halt z.5)))])
       (test-begin
         (check-not-exn (thunk (assign-registers p1))))
       (test-match
        (assign-registers p1)
        `(module
             ,info
             (begin
               (set! v.1 1)
               (set! w.2 46)
               (set! x.3 v.1)
               (set! x.3 (+ x.3 7))
               (set! y.4 x.3)
               (set! y.4 (+ y.4 4))
               (set! z.5 x.3)
               (set! z.5 (+ z.5 w.2))
               (set! t.6 y.4)
               (set! t.6 (* t.6 -1))
               (set! z.5 (+ z.5 t.6))
               (halt z.5)))
        (validate-assignments locals conflicts (info-ref info 'assignment)))))

   (test-case
     "Single variable with all registers"
     (check-match
      (assign-registers
       '(module
          ((locals (x.1))
           (conflicts ((x.1 ()))))
          (begin
            (set! x.1 10)
            (halt x.1))))
      `(module
         (,_ ... (assignment ((x.1 ,r))) ,_ ...)
         (begin
           (set! x.1 10)
           (halt x.1)))
      (register? r)))

   (test-case
     "Single variable spill"
     (check-match
      (parameterize ([current-assignable-registers '()])
        (assign-registers
         '(module
            ((locals (x.1))
             (conflicts ((x.1 ()))))
            (begin
              (set! x.1 10)
              (halt x.1)))))
      `(module
         (,_ ... (assignment ((x.1 ,fvar))) ,_ ...)
         (begin
           (set! x.1 10)
           (halt x.1)))
      (fvar? fvar)))

   (test-case
     "Single conflict without spill"
     (let ([l '(x.1 x.2)]
           [c '((x.2 (x.1)) (x.1 (x.2)))]
           [regs '(r9 r10)])
       (parameterize ([current-assignable-registers regs])
         (let ([p (assign-registers
                   `(module
                      ((locals ,l)
                       (conflicts ,c))
                      (begin
                        (set! x.1 10)
                        (set! x.2 5)
                        (halt x.1))))])
           (check-match
            p
            `(module
               (,_ ... (assignment (,a ...)) ,_ ...)
               (begin
                 (set! x.1 10)
                 (set! x.2 5)
                 (halt x.1)))
            (validate-assignments l c a regs))))))

   (test-case
       "Single conflict with spill"
     (let ([l '(x.1 x.2)]
           [c '((x.2 (x.1)) (x.1 (x.2)))]
           [regs '()])
       (parameterize ([current-assignable-registers regs])
         (let ([p (assign-registers
                   `(module
                      ((locals ,l)
                       (conflicts ,c))
                      (begin
                        (set! x.1 5)
                        (set! x.2 10)
                        (halt x.1))))])
           (check-match
            p
            `(module
               (,_ ... (assignment (,a ...)) ,_ ...)
               (begin
                 (set! x.1 5)
                 (set! x.2 10)
                 (halt x.1)))
            (validate-assignments l c a regs))))))

   (test-case
     "Multiple variables without spilling"
     (let ([p (assign-registers test-prog1)])
       (check-match
        p
        `(module
           (,_ ... (assignment (,a ...)) ,_ ...)
           (begin
             (set! x.1 1)
             (set! x.2 2)
             (set! x.3 3)
             (set! x.3 (+ x.3 x.2))
             (set! x.3 (+ x.3 x.1))
             (halt x.3)))
        (validate-assignments test-locals1 test-conflicts1 a (current-assignable-registers)))))

   (test-case
     "Multiple variables with one spill"
     (let ([regs '(r9 r10)])
       (parameterize ([current-assignable-registers regs])
         (let ([p (assign-registers test-prog1)])
           (check-match
            p
            `(module
               (,_ ... (assignment (,a ...)) ,_ ...)
               (begin
                 (set! x.1 1)
                 (set! x.2 2)
                 (set! x.3 3)
                 (set! x.3 (+ x.3 x.2))
                 (set! x.3 (+ x.3 x.1))
                 (halt x.3)))
            (validate-assignments test-locals1 test-conflicts1 a regs))))))

   (test-case "Multiple variables with spilling"
     (let ([regs '(rcx)])
       (parameterize ([current-assignable-registers regs])
         (let ([p (assign-registers test-prog1)])
           (check-match
            p
            `(module
               (,_ ... (assignment (,a ...)) ,_ ...)
               (begin
                 (set! x.1 1)
                 (set! x.2 2)
                 (set! x.3 3)
                 (set! x.3 (+ x.3 x.2))
                 (set! x.3 (+ x.3 x.1))
                 (halt x.3)))
            (validate-assignments test-locals1 test-conflicts1 a regs))))))

   (test-case "Multiple variables without spilling"
     (let ([regs (set-remove (current-assignable-registers) 'rcx)])
       (parameterize ([current-assignable-registers regs])
         (let ([p (assign-registers test-prog2)])
           (check-match
            p
            `(module
               (,_ ... (assignment (,a ...)) ,_ ...)
               (begin
                 (set! x.1 10)
                 (set! x.2 15)
                 (set! x.1 (+ x.1 x.2))
                 (set! x.3 5)
                 (set! x.2 (+ x.2 x.1))
                 (set! x.2 (+ x.2 x.3))
                 (halt x.1)))
            (validate-assignments test-locals2 test-conflicts2 a regs))))))

   (test-case "Multiple variables with spilling"
     (let ([regs '(rcx rsi)])
       (parameterize ([current-assignable-registers regs])
         (let ([p (assign-registers test-prog2)])
           (check-match
            p
            `(module
               (,_ ... (assignment (,a ...)) ,_ ...)
               (begin
                 (set! x.1 10)
                 (set! x.2 15)
                 (set! x.1 (+ x.1 x.2))
                 (set! x.3 5)
                 (set! x.2 (+ x.2 x.1))
                 (set! x.2 (+ x.2 x.3))
                 (halt x.1)))
            (validate-assignments test-locals2 test-conflicts2 a regs))))))))

(define (a3-conflict-analysis-tests conflict-analysis)
  (test-suite
   "a3 conflict-analysis tests"

   (test-case "Single variable"
     (check-match
      (conflict-analysis
       '(module
          ((locals (x.1))
           (undead-out ((x.1) ())))
          (begin
            (set! x.1 10)
            (halt x.1))))
      `(module
         (,_ ... (conflicts ((x.1 ()))) ,_ ...)
         (begin
           (set! x.1 10)
           (halt x.1)))))

   (test-case "Simple case for two variables"
     (with-check-info (['conflicts (string-info "Conflicts of the form: (conflicts ((x.1 (x.2)) (x.2 (x.1)))")])
       (let ([l '(x.1 x.2)]
             [u '((x.1) (x.1) ())]
             [e1 '((x.1 (x.2)) (x.2 (x.1)))])
         (check-match
          (conflict-analysis
           `(module
              ((locals ,l)
               (undead-out ,u))
              (begin
                (set! x.1 5)
                (set! x.2 10)
                (halt x.1)) ))
          `(module
             (,_ ... (conflicts (,c ...)) ,_ ...)
             (begin
               (set! x.1 5)
               (set! x.2 10)
               (halt x.1)))
          (validate-conflicts l c e1)))))

   (test-case "Complex case for two variables"
     (with-check-info (['conflicts (string-info "Conflicts of the form: (conflicts ((x.1 (x.2)) (x.2 (x.1))))")])
       (let ([l '(x.1 x.2)]
             [u '((x.2 x.1) (x.2 x.1) (x.1) ())]
             [e1 '((x.1 (x.2)) (x.2 (x.1)))])
         (check-match
          (conflict-analysis
           `(module
              ((locals ,l)
               (undead-out ,u))
              (begin
                (set! x.1 5)
                (set! x.2 (+ x.2 x.1))
                (set! x.1 (+ x.1 x.2))
                (halt x.1))))
          `(module
             (,_ ... (conflicts (,c ...)) ,_ ...)
             (begin
               (set! x.1 5)
               (set! x.2 (+ x.2 x.1))
               (set! x.1 (+ x.1 x.2))
               (halt x.1)))
          (validate-conflicts l c e1)))))

   (test-case "Simple case for three variables"
     (with-check-info (['conflicts (string-info "Conflicts of the form: (conflicts ((x.3 (x.2 x.1)) (x.1 (x.2 x.3)) (x.2 (x.1 x.3))))")])
       (let ([l '(x.1 x.2 x.3)]
             [u '((x.1) (x.2 x.1) (x.2 x.1) (x.3 x.1) (x.3) ())]
             [e '((x.3 (x.2 x.1)) (x.1 (x.2 x.3)) (x.2 (x.1 x.3)))])
         (check-match
          (conflict-analysis
           `(module
              ((locals ,l)
               (undead-out ,u))
              (begin
                (set! x.1 1)
                (set! x.2 2)
                (set! x.3 3)
                (set! x.3 (+ x.3 x.2))
                (set! x.3 (+ x.3 x.1))
                (halt x.3))))
          `(module
             (,_ ... (conflicts (,c ...)) ,_ ...)
             (begin
               (set! x.1 1)
               (set! x.2 2)
               (set! x.3 3)
               (set! x.3 (+ x.3 x.2))
               (set! x.3 (+ x.3 x.1))
               (halt x.3)))
          (validate-conflicts l c e)))))

   (test-case "Complex case for multiple variables"
     (with-check-info (['conflicts (string-info "Conflicts of the form: (conflicts ((b.2 (a.1 c.3)) (d.4 (a.1 c.3)) (c.3 (a.1 d.4 b.2)) (a.1 (c.3 d.4 b.2))))")])
       (let ([l '(a.1 b.2 c.3 d.4)]
             [u '((a.1) (a.1 c.3) (a.1 c.3 b.2) (a.1 c.3) (c.3) (d.4) ())]
             [e '((b.2 (a.1 c.3)) (d.4 (c.3)) (c.3 (a.1 d.4 b.2)) (a.1 (c.3 b.2)))])
         (check-match
          (conflict-analysis
           `(module
              ((locals ,l)
               (undead-out ,u))
              (begin
                (set! a.1 1)
                (set! c.3 2)
                (set! b.2 a.1)
                (set! b.2 (+ b.2 c.3))
                (set! d.4 a.1)
                (set! d.4 (* d.4 c.3))
                (halt d.4))))
          `(module
             (,_ ... (conflicts (,c ...)) ,_ ...)
             (begin
               (set! a.1 1)
               (set! c.3 2)
               (set! b.2 a.1)
               (set! b.2 (+ b.2 c.3))
               (set! d.4 a.1)
               (set! d.4 (* d.4 c.3))
               (halt d.4)))
          (validate-conflicts l c e)))))))

(define (a3-public-test-suite
         passes
         interp-paren-x64
         check-paren-x64
         interp-values-lang
         check-values-lang
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
         assign-homes
         flatten-begins
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
   "a3 public test suite"
   #:before
   (thunk
    (current-run/read nasm-run/print-number))
   #:after
   (thunk
    (current-run/read run/read))

   (test-suite
    "a3 tests"
    (a3-public-test-undead-sets undead-analysis)
    (a3-conflict-analysis-tests conflict-analysis)
    (a3-assign-registers-stress-tests assign-registers))))
