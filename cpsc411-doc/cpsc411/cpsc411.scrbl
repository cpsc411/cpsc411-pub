#lang scribble/manual

@title[#:style 'toc]{CPSC 411 Infrastructure Package}
@author[@author+email["William J. Bowman" "wjb@williamjbowman.com"]]

This package contains the CPSC 411 project infrastructure, including testing
tools and test suites, compiler implementation support and parameters, and
run-time system.

@(local-table-of-contents #:style 'immediate-only)

@include-section{scribblings/langs.scrbl}
@include-section{scribblings/compiler-lib.scrbl}
@include-section{scribblings/2c-run-time.scrbl}
@include-section{scribblings/ptr-run-time.scrbl}
@include-section{scribblings/info-lib.scrbl}
@include-section{scribblings/graph-lib.scrbl}
@include-section{scribblings/test-lang.scrbl}
