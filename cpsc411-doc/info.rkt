#lang info
(define collection 'multi)
(define deps '(("base" #:version "7.3")))
(define build-deps
  '(("base" #:version "7.3")
    "scribble-lib"
    "racket-doc"
    "sandbox-lib"
    "cpsc411-lib"))
(define pkg-desc "Documentation for CPSC411 library.")
(define pkg-authors '(wilbowma))
