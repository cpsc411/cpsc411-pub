#lang racket/base
(require cpsc411/langs/v8
         rackunit)

(check-false
 (interp-exprs-lang-v8 '(module (call pair? (error 0)))))

(check-true
 (interp-exprs-lang-v8 '(module (call error? (error 0)))))

(check-false
 (interp-exprs-lang-v8 '(module (call error? (cons 0 1)))))
