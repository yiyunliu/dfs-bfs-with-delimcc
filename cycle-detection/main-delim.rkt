#lang racket

(require racket/control)

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
  (reset
   (let loop ([node node])
     (define visit? (shift k (Discovered node k)))
     (when visit?
       (for ([target (neighbors graph node)])
         (loop target)))
     (shift k (Processed node k))
     'Done)))

(define (node-cyclic? graph node)
  (define size (vector-length graph))
  (define discovered (make-vector size #f))
  (define processed (make-vector size #f))
  (let/cc return
    (let loop ([event (dfs-node graph node)])
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


(provide node-cyclic?)
