#lang scribble/manual

@(require
  (for-label
   cpsc411/graph-lib
   racket/base
   racket/contract)
  scribble/eval)

@(define eg
   (make-base-eval "(require cpsc411/graph-lib)"))

@title{Functional Graphs}
@defmodule[cpsc411/graph-lib]
This library defines a functional graph library, where graphs are represented as
lists of proper association lists for simple serialization.

@defproc[(graph? [v any/c]) boolean?]{
A predicates that returns @racket[#t] if @racket[v] is a graph.

A graph is an proper association list from a vertex to a list of vertexes.
A directed edge exists between a vertex V1 to V2 if V2 is in the list associated
with V1.

@examples[#:eval eg
(graph? '())
(graph? '((x ())))
(graph? '((x (y))))
(graph? '((x . (y))))
(graph? '((x (y)) (y (x))))
]
}

@defproc[(new-graph [vs (listof any/c) '()]) graph?]{
Returns a new graph, initialized to include each element of @racket[vs] as
a vertex in the graph.

@examples[#:eval eg
(new-graph)
(new-graph '(a b c))
]
}

@defproc[(add-vertex [g graph?] [v any/c]) graph?]{
Returns a new graph equivalent to @racket[g] with @racket[v] added a vertex.

@examples[#:eval eg
(add-vertex (new-graph) 'a)
]
}

@defproc[(remove-vertex [g graph?] [v any/c]) graph?]{
Returns a new graph equivalent to @racket[g] twith the vertex @racket[v] removed from the graph.

@examples[#:eval eg
(add-vertex (new-graph) 'a)
(remove-vertex (add-vertex (new-graph) 'a) 'a)
(add-directed-edge (new-graph '(a b)) 'a 'b)
(remove-vertex (add-directed-edge (new-graph '(a b)) 'a 'b) 'b)
]
}

@defproc[(add-directed-edge [g graph?] [v any/c] [u any/c]) graph?]{
Returns a new graph equivalent to @racket[g] with a directed edge from
@racket[v] to @racket[u].
Assumes both @racket[v] and @racket[u] are already vertexes in the graph.

@examples[#:eval eg
(add-directed-edge (new-graph '(a b)) 'a 'b)
]
}

@defproc[(add-edge [g graph?] [v any/c] [u any/c]) graph?]{
Returns a new graph equivalent to @racket[g] with a directed edge from
@racket[v] to @racket[u], and a directed edge from @racket[u] to @racket[v].
Assumes both @racket[v] and @racket[u] are already vertexes in the graph.

@examples[#:eval eg
(add-edge (new-graph '(a b)) 'a 'b)
]
}

@defproc[(add-directed-edges [g graph?] [v any/c] [us (listof any/c)]) graph?]{
Returns a new graph equivalent to @racket[g] with a directed edge from
@racket[v] to every element of @racket[us].
Assumes all vertexes are already in @racket[g].

@examples[#:eval eg
(add-directed-edges (new-graph '(a b c d)) 'a '(b c))
]
}

@defproc[(add-edges [g graph?] [v any/c] [us (listof any/c)]) graph?]{
Returns a new graph equivalent to @racket[g] with a directed edge from
@racket[v] to every element of @racket[us], and from every element of
@racket[us] to @racket[v].
Assumes all vertexes are already in @racket[g].

@examples[#:eval eg
(add-edges (new-graph '(a b c d)) 'a '(b c))
]
}

@defproc[(get-neighbors [g graph?] [v any/c]) graph?]{
Returns a list of each vertex with an edge coming from @racket[v] in the graph
@racket[g].
Returns the empty set if @racket[v] in the graph.

@examples[#:eval eg
(add-edges (new-graph '(a b c d)) 'a '(b c))
(get-neighbors (add-edges (new-graph '(a b c d)) 'a '(b c)) 'a)
]
}
