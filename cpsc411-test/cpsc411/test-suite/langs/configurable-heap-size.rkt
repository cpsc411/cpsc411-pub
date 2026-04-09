#lang racket/base

(require rackunit)
(require cpsc411/langs/v8)
(require (only-in cpsc411/langs/base current-heap-size))

(check-exn 
  (lambda ()
    (interp-asm-pred-lang-v8
      '(module
         ((new-frames (() ())))
         (begin
           (set! x.1 r12)
           (set! r12 (+ r12 10008))
           (mset! x.1 10000 5))))))

(check-not-exn 
  (lambda ()
    (current-heap-size 20000)
    (interp-asm-pred-lang-v8
      '(module
         ((new-frames (() ())))
         (begin
           (set! x.1 r12)
           (set! r12 (+ r12 10008))
           (mset! x.1 10000 5))))))
