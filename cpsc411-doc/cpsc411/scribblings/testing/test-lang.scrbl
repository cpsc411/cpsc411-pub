#lang scribble/manual

@(require
  (for-label racket/base)
  scribble/examples)

@title{Test Lang}
@defmodulelang[cpsc411/test-lang]
@deprecated

This module defines a language with a small reader extension to simplify writing
future-proof tests.

The language adds the reader macro @as-index{@litchar{$}}, which behaves like
@litchar{,} (@racket[unquote]), but calls the @racket[current-input-encoder].
to encode a Racket value into a valid value for the CPSC411 compiler.
For much of the compiler, this is simply the identity function: Racket numbers
in the int64 range are also CPSC411 values.
Once we add tagged datatypes, this encoding changes.

You can use @racketmodname[cpsc411/test-lang] only as a @litchar{#lang}.

For example, the following module
@codeblock{
#lang cpsc411/test-lang
`(begin (set! rax 1))
`(begin (set! rax $1))
'(begin (set! rax $1))
(parameterize ([current-input-encoder (lambda (x) (* x 8))])
  `(begin (set! rax $1)))
}

Reads the same as:
@(define eg (make-base-eval))
@examples[#:eval eg
(require
 cpsc411/test-suite/utils)
`(begin (set! rax 1))
`(begin (set! rax ,((current-input-encoder) 1)))
'(begin (set! rax ,((current-input-encoder) 1)))
(parameterize ([current-input-encoder (lambda (x) (* x 8))])
  `(begin (set! rax ,((current-input-encoder) 1))))
]

