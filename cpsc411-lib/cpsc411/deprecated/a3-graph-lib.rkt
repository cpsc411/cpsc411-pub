#lang racket
(provide (all-defined-out))

;; ------------------------------------------------------------------------
;; Functional Graph library.

;; A graph is represented as an association list from vertex to a list of vertex.
;; A directed edge exists between a vertex V1 to V2 if V2 is in the list associated with
;; V1.
;; An undirected edge exists between V1 and V2 if there exists a directed edge
;; between V1 and V2 and between V2 and V1.

; -> Graph
; or
; List-of Vertex -> Graph
; Returns a new graph.
; When provided the optional argument vs, initializes the graph to include the
; each Vertex in vs.
(define (new-graph [vs '()])
  (for/fold ([g '()])
            ([v vs])
    (add-vertex g v)))

; Graph -> Vertex -> Graph
; Returns a new graph that is identical to g but with the Vertex v added.
(define (add-vertex g v)
  (cons `(,v ()) g))

; Graph -> Vertex -> Graph
; Returns a new graph that is identical to g but with the Vertex v1 removed.
(define (remove-vertex g v1)
  (for/fold ([g '()])
            ([(k v) (in-dict (dict-remove g v1))])
    (dict-set g k (list (remove v1 (car v))))))

; Graph -> Vertex -> Vertex -> Graph
; Returns a new graph identical to g with a directed edge from u to v added.
; Assumes u and v have already been added as vertexes.
(define (add-directed-edge g u v)
  ; Terribly inefficient...
  (dict-set g u (list (set->list (set-add (list->set (car (dict-ref g u))) v)))))

; Graph -> Vertex -> Vertex -> Graph
; Returns a new graph identical to g with an edge from u to v and vice versa.
(define (add-edge g u v)
  (add-directed-edge (add-directed-edge g u v) v u))

; Graph -> Vertex -> (List-of Vertex) -> Graph
; Returns a new graph identical to g with a directed edge from u to each vertex
; in vs.
(define (add-directed-edges g u vs)
  (for/fold ([g g])
            ([v vs])
    (add-directed-edge g u v)))

; Graph -> Vertex -> (List-of Vertex) -> Graph
; Returns a new graph identical to g with edges from u to each vertex
; in vs, and vice versa.
(define (add-edges g u vs)
  (for/fold ([g g])
            ([v vs])
    (add-edge g u v)))

; Graph -> Vertex -> List of Vertex
; Return each a list of each vertex with an edge coming from v in the graph g.
(define (get-neighbors g v)
  (car (dict-ref g v)))

(module+ test
  (require rackunit)
  (check-equal?
   (new-graph)
   '())
  (check-equal?
   (new-graph '(v x))
   '((x ()) (v ())))
  (check-equal?
   (add-directed-edge (new-graph '(v x)) 'x 'v)
   '((x (v)) (v ())))
  (check-equal?
   (add-edge (new-graph '(v x)) 'x 'v)
   '((x (v)) (v (x))))
  (check-equal?
   (remove-vertex (new-graph '(v x)) 'v)
   '((x ()))))
