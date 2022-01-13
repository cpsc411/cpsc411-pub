#lang info
(define collection 'multi)
(define deps
  '("rackunit-lib"
    "memoize-lib"
    "redex-lib"
    ("scribble-bettergrammar-lib" #:version "1.4.1")
    "scribble-lib"
    ("base" #:version "7.3")))

(define build-deps
  '("at-exp-lib"
    "rackunit-lib"))

(define pkg-desc "implementation (no documentation, tests) of the cpsc411 support library")
(define version "0.1")
(define pkg-authors '(wilbowma))
