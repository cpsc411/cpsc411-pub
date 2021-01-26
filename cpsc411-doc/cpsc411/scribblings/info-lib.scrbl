#lang scribble/manual

@(require
  (for-label
   cpsc411/info
   racket/base
   racket/contract)
  scribble/eval)

@(define cpsc411-eval
  (make-base-eval "(require cpsc411/compiler-lib)"))

@title{Info Data Structure}
@defmodule[cpsc411/info-lib]
This library defines the @tech{info} datatype.
An @deftech{info} is a datatype used to represent dictionaries or maps in a
convenient readable way.
It is similar to an association list, but uses proper lists rather than pairs.
This makes it more memory intensive, but means we do not need to introduce
dotted pair notation.

@defproc[(info? [v any/c]) any/c]{
Returns @racket[#t] if @racket[v] is an @tech{info}, and @racket[#f] otherwise.

@examples[#:eval cpsc411-eval
(info? '())
(info? '((a . 5)))
(info? '((a 5)))]
}

@defform[(info/c (key-name spec) ...)]{
Creates an @tech{info} @racket[contract?], specifying that the @tech{info} must
contain a mapping for each @racket[key-name], and the value for each key must
satsify @racket[spec].
The @racket[spec] language is roughly the same as the BNF pattern language used
in this course, except all terminals must be @racket[contract?]s.

@examples[#:eval cpsc411-eval
(require racket/contract)
((info/c) '())
((info/c) 5)
((info/c (locals (aloc? ...))) '())
((info/c (locals (aloc? ...))) '((locals ())))
((info/c (locals (aloc? ...))) '((locals (x.1))))
((info/c (locals (aloc? ...))) '((locals (5))))
((info/c (locals (aloc? ...)))
 '((locals (x.1)) (assignments ((x.1 5)))))
(define loc? (or/c register? fvar?))
((info/c (locals (aloc? ...)) (assignments ((aloc? loc?) ...)))
 '((locals (x.1)) (assignments ((x.1 5)))))
((info/c (locals (aloc? ...)) (assignments ((aloc? loc?) ...)))
'((locals (x.1)) (assignments ((x.1 rax)))))
]
}

@defproc[(info-ref [v info?] [key any/c]) (or/c #f any/c)]{
Returns the value associated with @racket[key] in @racket[v].

@examples[#:eval cpsc411-eval
(info-ref '((assignment ((x.1 rbx) (y.2 r)))) 'assignment)
(info-ref
  (info-ref '((assignment ((x.1 rbx) (y.2 r)))) 'assignment)
  'x.1)
]
}

@defproc[(info-set [v info?] [key any/c] [value any/c]) info?]{
Adds a mapping from @racket[key] to @racket[value] in the info @racket[v].

@examples[#:eval cpsc411-eval
(info-set '() 'x.1 'rbx)
(info-set (info-set '() 'x.1 'rbx) 'y.2 'r9)
(info-set '() 'assignment '((x.1 rbx) (y.2 r9)))
]
}

@defproc[(info-remove [v info?] [key any/c]) info?]{
Removed a mapping for @racket[key] in the info @racket[v].
}
