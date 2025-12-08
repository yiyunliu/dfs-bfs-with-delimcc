#lang racket

(require racket/control)
(require data/gvector)

(define graph?
  (vectorof (listof natural?) #:flat? #t))

(define (make-graph size edges)
  (define result (make-vector size '()))
  (for ([edge edges])
    (match-define (cons source target) edge)
    (vector-set! result source (cons target (vector-ref result source))))
  result)


(define (neighbors graph node)
  (vector-ref graph node))

;; Events
(define-struct Discovered (node cont) #:transparent)
(define-struct Processed (node cont) #:transparent)
;; singleton 'Done

(define (dfs-node graph node)
  (let loop ([node node])
    (define visit? (shift k (Discovered node k)))
    (when visit?
      (for ([target (neighbors graph node)])
        (loop target))
      (shift k (Processed node k)))
    'Done))

(define (dfs-graph graph)
  (for ([node (in-range (vector-length graph))])
    (dfs-node graph node))
  'Done)

;; Leetcode 207
(define (can-finish num-courses prereqs)
  (define graph (make-graph num-courses (map (lambda (x) (cons (first x) (second x))) prereqs)))
  (not (graph-cyclic? graph)))

(define (graph-cyclic? graph)
  (define size (vector-length graph))
  (define discovered (make-vector size #f))
  (define processed (make-vector size #f))
  (let/cc return
    (let loop ([event (reset (dfs-graph graph))])
      (match event
        [(Discovered node k)
         (cond
           ;; node has been processed
           [(vector-ref processed node)
            (loop (k #f))]
           ;; node has been discovered but not processed
           ;; graph is cyclic
           [(vector-ref discovered node)
            (return #t)]
           ;; node is undiscovered
           ;; mark as discovered and tell the generator to not skip
           [else
            (vector-set! discovered node #t)
            (loop (k #t))])]
        [(Processed node k)
           ;; node is fully processed
           ;; mark it as such and continue
         (vector-set! processed node #t)
         (loop (k))]
        ['Done
         (return false)]))))


;; Leetcode 210
(define (find-order num-courses prereqs)
  (define graph (make-graph num-courses (map (lambda (x) (cons (first x) (second x))) prereqs)))
  (topological-sort graph))

(define (topological-sort graph)
  (define size (vector-length graph))
  (define discovered (make-vector size #f))
  (define processed (make-vector size #f))
  (let/cc return
    (define courses (gvector))
    (let loop ([event (reset (dfs-graph graph))])
      (match event
        [(Discovered node k)
         (cond
           ;; node has been processed
           [(vector-ref processed node)
            (loop (k #f))]
           ;; node has been discovered but not processed
           ;; graph is cyclic
           [(vector-ref discovered node)
            (return '())]
           ;; node is undiscovered
           ;; mark as discovered and tell the generator to not skip
           [else
            (vector-set! discovered node #t)
            (loop (k #t))])]
        [(Processed node k)
           ;; node is fully processed
           ;; mark it as such and continue
         (vector-set! processed node #t)
         (gvector-add! courses node)
         (loop (k))]
        ['Done
         (return (gvector->list courses))]))))


(provide graph-cyclic? graph? make-graph can-finish find-order)
