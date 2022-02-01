#lang racket/base

(require
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

(for ([(interp tests) (in-dict test-prog-dict)])
  (for ([test tests])
    (with-check-info (['test-name (car test)]
                      ['test-prog (cadr test)]
                      ['interp (object-name interp)])
      (test-not-exn "Test test valid"
                    (lambda () ((dict-ref validator-dict interp) (cadr test))))
      (test-not-exn "Test test interps"
                    (lambda () (interp (cadr test)))))))
