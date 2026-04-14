#lang racket/base
(require cpsc411/langs/v11
         rackunit)

(check-false
 (interp-racketish-surface '(module (pair? (error 0)))))

(check-true
 (interp-racketish-surface '(module (error? (error 0)))))

(check-false
 (interp-racketish-surface '(module (error? (cons 0 1)))))

(check-false
  (interp-racketish-surface '(module (call not (call #t)))))
