#lang racket

(require racket/generator)
(require racket/trace)

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
(define-struct Discovered ([node]) #:transparent)
(define-struct Processed ([node]) #:transparent)

(define (dfs-node graph node)
  (generator
   ()
   (let loop ([node node])
     (yield (Discovered node))
     (println "before calling yield")
     (when (yield)
       (println "received true")
       (for ([target (neighbors graph node)])
         (loop target)))
     (println "done calling yield")
     (yield (Processed node))
     (void))))

(define (node-cyclic? graph node)
  (define size (vector-length graph))
  (define discovered (make-vector size #f))
  (define processed (make-vector size #f))
  (let/cc return
    (begin
      (define gen (dfs-node graph node))
      (let loop ()
        (println "calling gen without arg")
        (define result (gen))
        (println "done calling gen without arg")
        (match result
          ;; done
          [(? void?) (void)]

          ;; discovered the node
          [(Discovered node)
           (cond
             ;; node has been processed
             [(vector-ref processed node)
              (println "here0")
              ;; tell the generator to skip
              (gen #t)]
             ;; node has been discovered but not processed
             ;; graph is cyclic
             [(vector-ref discovered node)
              (println "here1")
              (return #t)]
             ;; node is undiscovered
             ;; mark as discovered and tell the generator to not skip
             [else
              (println "calling gen with #f")
              (gen #f)
              (println "done calling gen with #f")
              (vector-set! discovered node #t)
              (loop)])]
          [(Processed node)
           ;; node is fully processed
           ;; mark it as such and continue
           (println "here3")
           (vector-set! processed node #t)
           (loop)]))
      #f)))
