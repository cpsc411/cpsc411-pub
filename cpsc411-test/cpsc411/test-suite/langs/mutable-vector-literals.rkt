#lang racket
(require rackunit
         cpsc411/langs/v11)

(check-equal?
  (interp-racketish-surface
    '(module
       (let ([x #(0 1 2)])
         (vector-set! x 0 1)
         x)))
  #(1 1 2))
