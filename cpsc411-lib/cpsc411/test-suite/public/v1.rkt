#lang reader "../../test-lang/lang/reader.rkt"

(require
  racket/set
  racket/match
  racket/function
  racket/list
  rackunit
  "../utils.rkt"
  "../../langs/v1.rkt"
  cpsc411/compiler-lib)

(provide
 (all-defined-out))

(define paren-x64-v1-progs
  ;; string? x paren-x64-v1?
  ;; a name and a test program
  ;; valued define by langs/v1 interpreter
  `((""
     (begin (set! rax 5)))

    (""
     (begin
       (set! rdi 5)
       (set! rax rdi)))

    (""
     (begin
       (set! rax 4)
       (set! rax (+ rax 1))))

    (""
     (begin
       (set! rax 4)
       (set! rax (* rax 2))))

    ("set! register <= 2^63 - 1"
     (begin (set! rax ,(- (expt 2 63) 1))))

    ("set! register >= -2^63"
     (begin (set! rax ,(- (expt 2 63)))))

    ("add value <= 2^31 - 1"
     (begin
       (set! rax 0)
       (set! rax (+ rax ,(- (expt 2 31) 1)))))

    ("add value >= -2^31"
     (begin
       (set! rax 0)
       (set! rax (+ rax ,(- (expt 2 31))))))

    ("may multiply by value <= 2^31 - 1"
     (begin
       (set! rax 0)
       (set! rax (+ rax ,(- (expt 2 31) 1)))))

    ("may multiply by value >= -2^31"
     (begin
       (set! rax 0)
       (set! rax (+ rax ,(- (expt 2 31))))))

    ("integer overflow when adding"
     (begin
       (set! rax ,(max-int 64))
       (set! rax (+ rax 1))))

    ("integer underflow when adding"
     (begin
        (set! rax ,(min-int 64))
        (set! rax (+ rax -1))))

    ("integer overflow when multiplying"
     (begin
       (set! rax ,(expt 2 32))
       (set! rcx ,(expt 2 32))
       (set! rax (* rax rcx))))

    ("integer underflow when multiplying"
     (begin
       (set! rax ,(expt 2 32))
       (set! rcx ,(- (expt 2 32)))
       (set! rax (* rax rcx))))

    (""
     (begin
       (set! r9 $0)
       (set! r12 $0)
       (set! r9 (* r9 r12)) ; 0
       (set! r9 (* r9 $0)) ; 0
       (set! r9 (+ r9 $1)) ; 1
       (set! rcx r9) ; 1
       (set! rcx (* rcx $1)) ; 1
       (set! rax $7)
       (set! rax (+ rax rcx)) ; 8
       (set! rax (+ rax rax)) ; 16
       (set! r14 $2)
       (set! r14 (+ r14 rax)) ; 18
       (set! rax (* rax r14))))

    (""
     (begin
       (set! r11 $0)
       (set! rbp $2)
       (set! rcx $0)
       (set! rsi r11)
       (set! rsi (+ rsi rbp))
       (set! rsi (+ rsi $0))
       (set! rdi rsi)
       (set! rdi (+ rdi rcx))
       (set! rax rdi)))))

(hash-set! test-prog-dict interp-paren-x64-v1 (list->mutable-set paren-x64-v1-progs))

(define (a1-check-paren-x64-syntax-test-suite check-paren-x64)
  (test-suite
   "a1 check-syntax tests"

   (test-suite
    "test that bad syntax is rejected"

    (let ([x `(set! rax 5)])
      (test-validator-exn "must have a begin" check-paren-x64 x))

    (let ([x `(begin (set! rax "not a number!!!"))])
      (test-validator-exn "rejects patent nonsense" check-paren-x64 x))

    (let ([x `(begin
                (set! 5 4)
                (set! rax 0))])
      (test-validator-exn "set! must set a reg" check-paren-x64 x))

    (let ([x `(begin
                (set! rax 1)
                (set! rax (- rax rax)))])
      (test-validator-exn "binop must be valid" check-paren-x64 x))

    (let ([x `(begin
                (set! rcx 2)
                (set! rax (* rcx 1)))])
      (test-validator-exn "binop must involve set!'ed reg" check-paren-x64 x))

    (let ([x `(begin
                (set! rax 2)
                (set! rcx 2)
                (set! rax (* rcx rax)))])
      (test-validator-exn
       "binop must involve set!'ed reg on the lhs of the binop"
       check-paren-x64 x))

    (let ([x `(begin (set! rax ,(expt 2 63)))])
      (test-validator-exn "may not set! register > 2^63 - 1" check-paren-x64 x))

    (let ([x `(begin (set! rax ,(- (- (expt 2 63)) 1)))])
      (test-validator-exn "may not set! register < -2^63" check-paren-x64 x))

    (let ([x `(begin
                (set! rax 0)
                (set! rax (+ rax ,(expt 2 31))))])
      (test-validator-exn "may not add value > 2^31 - 1" check-paren-x64 x))

    (let ([x `(begin
                (set! rax 0)
                (set! rax (+ rax ,(- (- (expt 2 31)) 1))))])
      (test-validator-exn "may not add value < -2^31" check-paren-x64 x))

    (let ([x `(begin
                (set! rax 0)
                (set! rax (* rax ,(expt 2 31))))])
      (test-validator-exn
       "may not multiply by value > 2^31 - 1"
       check-paren-x64 x))

    (let ([x `(begin
                (set! rax 0)
                (set! rax (* rax ,(- (- (expt 2 31)) 1))))])
      (test-validator-exn
       "may not multiply by value < -2^31"
       check-paren-x64 x)))))

(define (a1-check-paren-x64-initialization-test-suite check-paren-x64)
  (test-suite
   "test that uninitialized rhs is rejected"

   (let ([x `(begin (set! rax rdi))])
     (test-validator-exn "in plain set!" check-paren-x64 x))

   (let ([x `(begin (set! rax (+ rax 10)))])
     (test-validator-exn "in add" check-paren-x64 x))

   (let ([x `(begin (set! rax (* rax 10)))])
     (test-validator-exn "in mul" check-paren-x64 x))

   (let ([x '(begin (set! rax (+ rax 10)))])
     (test-validator-exn "in add with immediate" check-paren-x64 x))

   (let ([x '(begin (set! rax (* rax 10)))])
     (test-validator-exn "in mul with immediate" check-paren-x64 x))

   (let ([x '(begin
               (set! rdi 1)
               (set! rax (+ rax rdi)))])
     (test-validator-exn "in add with reg on lhs" check-paren-x64 x))

   (let ([x '(begin
               (set! rax 1)
               (set! rax (+ rax rdi)))])
     (test-validator-exn "in add with reg on rhs" check-paren-x64 x))

   (let ([x '(begin
               (set! rdi 1)
               (set! rax (* rax rdi)))])
     (test-validator-exn "in mul with reg on rhs" check-paren-x64 x))

   (let ([x '(begin
               (set! rax 1)
               (set! rax (* rax rdi)))])
     (test-validator-exn "in mul with reg on lhs" check-paren-x64 x))))

(define (a1-interp-paren-x64-test-suite interp-paren-x64)
  (define decoder (current-actual-decoder))
  (define masker (current-expected-masker))
  (define encoder (current-input-encoder))
  (define run/read (current-run/read))

  (test-suite
   "interp-paren-x64 tests"
   #:before
   (thunk
    (current-run/read nasm-run/exit-code)
    (current-expected-masker exit-code-mask))
   #:after
   (thunk
    (current-run/read run/read)
    (current-expected-masker masker))

   (for/list ([i paren-x64-v1-progs])
     (test-case (first i)
       (check-equal?/upto (interp-paren-x64 (second i))
                          (interp-paren-x64-v1 (second i)))))))

(define (a1-paren-x64-v1-test-suite passes)
  (define pass-list (current-pass-list))
  (define decoder (current-actual-decoder))
  (define masker (current-expected-masker))
  (define encoder (current-input-encoder))
  (define run/read (current-run/read))

  (test-suite
   "compile paren-x64-v1 tests"
   #:before
   (thunk
    (current-pass-list passes)
    (current-run/read nasm-run/exit-code)
    (current-expected-masker exit-code-mask))
   #:after
   (thunk
    (current-pass-list pass-list)
    (current-run/read run/read)
    (current-expected-masker masker))

   (for/list ([i paren-x64-v1-progs])
     (test-case (first i)
       (check-equal?/upto (execute (second i))
                          (interp-paren-x64-v1 (second i)))))))

(define (v1-public-test-suite passes interp-list
                              check-paren-x64 interp-paren-x64)
  (define decoder (current-actual-decoder))
  (define masker (current-expected-masker))
  (define encoder (current-input-encoder))
  (define run/read (current-run/read))

  (test-suite
   "a1 public test suite"
   #:before
   (thunk
    (current-run/read nasm-run/exit-code)
    (current-expected-masker exit-code-mask))
   #:after
   (thunk
    (current-run/read run/read)
    (current-expected-masker masker))

   (test-suite
    "check-paren-x64 tests"

    (a1-check-paren-x64-syntax-test-suite check-paren-x64)
    (a1-check-paren-x64-initialization-test-suite check-paren-x64))

   (test-suite
    "testomatic test suite"
    (compiler-testomatic passes interp-list))))
