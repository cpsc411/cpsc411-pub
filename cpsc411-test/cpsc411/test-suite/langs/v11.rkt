#lang racket/base
(require cpsc411/langs/v11
         (only-in cpsc411/langs/base error?)
         rackunit)

(check-false
 (interp-racketish-surface '(module (pair? (error 0)))))

(check-true
 (interp-racketish-surface '(module (error? (error 0)))))

(check-false
 (interp-racketish-surface '(module (error? (cons 0 1)))))

(check-false
  (interp-racketish-surface '(module (call not (call #t)))))

(check-equal? (interp-racketish-surface '(module (error 67)))
              (interp-racketish-surface '(module (begin
                                                   (error 67)
                                                   (error 69)))))

(check-true
  (error? (interp-racketish-surface '(module (make-vector -1)))))
