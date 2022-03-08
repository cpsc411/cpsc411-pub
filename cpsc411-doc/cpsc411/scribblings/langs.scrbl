#lang scribble/manual

@(require scribble/example)

@title[#:style 'toc #:tag "ref-langs"]{Language Reference Implementations}

This library provides reference implementations of each language, including
interpreters and validators.
Valid programs should run correctly in the interpreters, although so will some
invalid programs.
Validators will reject invalid programs, but don't necessarily provide useful errors.

@include-section{langs/v1.scrbl}
@include-section{langs/v2.scrbl}
@include-section{langs/v2-reg-alloc.scrbl}
@include-section{langs/v3.scrbl}
@include-section{langs/v4.scrbl}
@include-section{langs/v5.scrbl}
@include-section{langs/v6.scrbl}
@include-section{langs/v6-5.scrbl}
@include-section{langs/v7.scrbl}
