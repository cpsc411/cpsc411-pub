#lang racket/base

;; Test suite: generate Haskell modules from v9 grammars and compile with GHC

(require racket/system
         racket/format
         racket/file
         "redex-gen.rkt"
         "haskell-gen.rkt"
         "v9.rkt")

(define test-dir "/tmp/haskell-gen-tests")
(define pass-count 0)
(define fail-count 0)

(define (ensure-test-dir)
  (unless (directory-exists? test-dir)
    (make-directory test-dir)))

(define (test-grammar name grammar-data)
  (let* ([hs-file (build-path test-dir (string-append (symbol->string name) ".hs"))]
         [o-file  (build-path test-dir (string-append (symbol->string name) ".o"))])
    (grammar->haskell-module/file grammar-data (path->string hs-file))
    (let ([ok (system (format "ghc -c ~a -o ~a 2>&1" (path->string hs-file) (path->string o-file)))])
      (if ok
          (begin
            (printf "  ~a: ✓\n" name)
            (set! pass-count (add1 pass-count)))
          (begin
            (printf "  ~a: ✗ FAILED\n" name)
            (printf "  --- generated ---\n")
            (display (file->string (path->string hs-file)))
            (printf "  --- end ---\n")
            (set! fail-count (add1 fail-count)))))))

(define all-grammars
  `((exprs-lang-v9             ,exprs-lang-v9-grammar-data)
    (exprs-unique-lang-v9      ,exprs-unique-lang-v9-grammar-data)
    (exprs-unsafe-data-lang-v9 ,exprs-unsafe-data-lang-v9-grammar-data)
    (exprs-unsafe-lang-v9      ,exprs-unsafe-lang-v9-grammar-data)
    (just-exprs-lang-v9        ,just-exprs-lang-v9-grammar-data)
    (lam-opticon-lang-v9       ,lam-opticon-lang-v9-grammar-data)
    (lam-free-lang-v9          ,lam-free-lang-v9-grammar-data)
    (closure-lang-v9           ,closure-lang-v9-grammar-data)
    (hoisted-lang-v9           ,hoisted-lang-v9-grammar-data)
    (proc-exposed-lang-v9      ,proc-exposed-lang-v9-grammar-data)))

(define (run-tests)
  (ensure-test-dir)
  (printf "Haskell generation tests (v9 grammars)\n")
  (printf "=======================================\n")
  (for-each (lambda (entry)
              (test-grammar (car entry) (cadr entry)))
            all-grammars)
  (printf "=======================================\n")
  (printf "~a passed, ~a failed\n" pass-count fail-count)
  (when (> fail-count 0)
    (exit 1)))

(run-tests)
