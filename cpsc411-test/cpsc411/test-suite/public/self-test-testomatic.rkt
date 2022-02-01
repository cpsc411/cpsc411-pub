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

#|
not running:
Guess it has something to do with compare?
(letrec ((L.cs411main.1
          (lambda ()
            (set! r11 7)
            (L.fib.1)))
         (L.fib.1
          (lambda ()
            (set! r9 0)
            (set! r10 1)
            (set! r12 r11)
            (L.fib_iter.1)))
         (L.fib_iter.1
          (lambda ()
            (compare r12 0)
            (jump-if = L.fib_done.1)
            (L.fib_not_done_yet.1)))
         (L.fib_not_done_yet.1
          (lambda ()
            (set! r13 r10)
            (set! r10 (+ r10 r9))
            (set! r9 r13)
            (set! r12 (+ r12 -1))
            (jump L.fib_iter.1)
            (L.fib_done.1)))
         (L.fib_done.1
          (lambda ()
            (set! rdi r10)
            (set! rax rdi))))
  (L.cs411main.1))
|#
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
