#lang reader "../../test-lang/lang/reader.rkt"

(require
  racket/match
  racket/function
  rackunit
  "../utils.rkt"
  "../../langs/v1.rkt"
  cpsc411/compiler-lib)

(provide
 (all-defined-out))

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
       check-paren-x64 x)))

   (test-suite
    "valid programs must be accepted"

    (let ([x `(begin (set! rax 5))])
      (test-begin (check-validator check-paren-x64 x)))

    (let ([x `(begin
                (set! rdi 5)
                (set! rax rdi))])
      (test-begin (check-validator check-paren-x64 x)))

    (let ([x `(begin
                (set! rax 4)
                (set! rax (+ rax 1)))])
      (test-begin (check-validator check-paren-x64 x)))

    (let ([x `(begin
                (set! rax 4)
                (set! rax (* rax 2)))])
      (test-begin (check-validator check-paren-x64 x)))

    (let ([x `(begin (set! rax ,(- (expt 2 63) 1)))])
      (test-validator "may set! register <= 2^63 - 1" check-paren-x64 x))

    (let ([x `(begin (set! rax ,(- (expt 2 63))))])
      (test-validator "may set! register >= -2^63" check-paren-x64 x))

    (let ([x `(begin
                (set! rax 0)
                (set! rax (+ rax ,(- (expt 2 31) 1))))])
      (test-validator "may add value <= 2^31 - 1" check-paren-x64 x))

    (let ([x `(begin
                (set! rax 0)
                (set! rax (+ rax ,(- (expt 2 31)))))])
      (test-validator "may add value >= -2^31" check-paren-x64 x))

    (let ([x `(begin
                (set! rax 0)
                (set! rax (+ rax ,(- (expt 2 31) 1))))])
      (test-validator "may multiply by value <= 2^31 - 1" check-paren-x64 x))

    (let ([x `(begin
                (set! rax 0)
                (set! rax (+ rax ,(- (expt 2 31)))))])
      (test-validator "may multiply by value >= -2^31" check-paren-x64 x)))))

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

(define (a1-paren-x64-test-suite passes interp-paren-x64)
  (define pass-list (current-pass-list))

  (test-suite
   "paren-x64 tests"
   #:before
   (thunk
    (current-pass-list passes))
   #:after
   (thunk
    (current-pass-list pass-list))

   (let ([x `(begin
               (set! rax ,(max-int 64))
               (set! rax (+ rax 1)))])
     (test-case "integer overflow when adding"
                (check-confluent?/upto (execute x) (interp-paren-x64 x) (min-int 64))))

   (let ([x `(begin
               (set! rax ,(min-int 64))
               (set! rax (+ rax -1)))])
     (test-case "integer underflow when adding"
                (check-confluent?/upto (execute x) (interp-paren-x64 x) (max-int 64))))

   (let ([x `(begin
               (set! rax ,(expt 2 32))
               (set! rcx ,(expt 2 32))
               (set! rax (* rax rcx)))])
     (test-case "integer overflow when multiplying"
                (check-confluent?/upto (execute x) (interp-paren-x64 x) 0)))

   (let ([x `(begin
               (set! rax ,(expt 2 32))
               (set! rcx ,(- (expt 2 32)))
               (set! rax (* rax rcx)))])
     (test-case "integer underflow when multiplying"
                (check-confluent?/upto (execute x) (interp-paren-x64 x) 0)))

   (let ([x `(begin
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
               (set! rax (* rax r14)))]) ; 18 * 16
     (test-begin
      (check-confluent?/upto (execute x) (interp-paren-x64 x) (* 18 16))))

   (let ([x `(begin
               (set! r11 $0)
               (set! rbp $2)
               (set! rcx $0)
               (set! rsi r11)
               (set! rsi (+ rsi rbp))
               (set! rsi (+ rsi $0))
               (set! rdi rsi)
               (set! rdi (+ rdi rcx))
               (set! rax rdi))])
     (test-begin
      (check-confluent?/upto (execute x) (interp-paren-x64 x) 2)))))

(define (a1-public-test-suite passes interp-paren-x64)
  (match-define
    (list
     check-paren-x64
     generate-x64
     wrap-x64-run-time
     wrap-x64-boilerplate)
    passes)

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
    "check compiler against reference interpreter"
    (a1-paren-x64-test-suite passes interp-paren-x64-v1))

   (test-suite
    "check compiler against milestone interpreter"
    (a1-paren-x64-test-suite passes interp-paren-x64))))
