#lang racket/base

(require
 racket/function
 racket/engine
 racket/dict
 rackunit
 cpsc411/test-suite/utils
 cpsc411/langs/v1
 cpsc411/langs/v2
 cpsc411/langs/v3
 cpsc411/langs/v4

 cpsc411/test-suite/public/v1
 cpsc411/test-suite/public/v2
 cpsc411/test-suite/public/v3
 cpsc411/test-suite/public/v4)

;; Expects to be used with check-not-exn or check-exn.
(define (check-with-timeout timeout proc)
  (let* ([e (engine proc)]
         [res (engine-run timeout e)])
    (unless res
      (fail "Test timed out"))
    (engine-result e)))

(for ([(interp tests) (in-dict test-prog-dict)])
  (for ([test tests])
    (with-check-info (['test-name (car test)]
                      ['test-prog (cadr test)]
                      ['interp (object-name interp)])
      (check-not-exn
       (thunk
        (check-with-timeout 1000 (lambda (_) ((dict-ref validator-dict interp) (cadr test))))))
      (check-not-exn
       (thunk (check-with-timeout 1000 (lambda (_) (interp (cadr test)))))))))

;; better timeout
(check-exn
 exn:fail?
 (thunk
  (check-with-timeout
  1000
  (thunk (interp-block-asm-lang-v4
          '(module
             (define L.x.1
               (begin
                 (set! rcx rcx)
                 (jump L.x.2)))

             (define L.x.2
               (begin
                 (set! rcx rcx)
                 (halt 0)))))))))
