#lang racket
(require "../common.rkt")
(require "../db.rkt")

; See ../breadcrumbs-poc.rkt
;
; Generate a set of expected responses suitable for unit testing a
; loader for databases from a breadcrumb file.
;
; Presume a loaded database generated from the batch preparation pipeline.

(define (log-one fn name-fn db . args)
;    (writeln `((event . log-one-begin) (name-fn . ,name-fn)))
    (let* ((
        v (apply fn (cons db args))
        ))
        (writeln `(
            (method-name . ,name-fn)
            (args . ,args)
            (expected . ,v)))
        v
        ))

(define (log-one-stream fn name-fn db . args)
;    (writeln `((event . log-one-begin) (name-fn . ,name-fn)))
    (let* ((
        v (stream->list (apply fn (cons db args)))
        ))
        (writeln `(
            (method-name . ,name-fn)
            (args . ,args)
            (expected . ,v)))
        v
        ))

(define (log-one-countn-v0 fn name-fn db num)
;   (writeln `((event . log-one-begin) (name-fn . ,name-fn)))
    (for ((i (in-range 0 num)))
        (let* (
            (v (fn db i))
            )
            (writeln `(
                (method-name . ,name-fn)
                (i . ,i)
                (expected . ,v)))
            )))

(define (log-one-countn fn name-fn db num)
;    (writeln `((event . log-one-begin) (method-name . ,name-fn)))
    (define (f1 i) (fn db i))
    (let* (
            (vs (map f1 (stream->list (in-range 0 num))))
            )
        (writeln `(
            (method-name . ,name-fn)
            (expecteds . ,vs)))
        ))

(define (run-db db)
    (let* (
        (categories (log-one db:category* 'db:category* db))
        (num-categories (vector-length categories))
        (_ (log-one db:predicate* 'db:predicate* db))
        (_ (log-one db:concept-cui-corpus 'db:concept-cui-corpus db))
        (_ (log-one testing:db:concept-cui-index 'db:concept-cui-index db))
        (_ (log-one testing:db:concept-name-corpus 'db:concept-name-corpus db))
        (_ (log-one testing:db:concept-name-index 'db:concept-name-index db))
        (_ (log-one-countn db:catid->cid* 'db:catid->cid* db num-categories))
        (concepts (log-one-stream db:cid&concept-stream 'db:cid&concept-stream db))
        (num-concepts (length concepts))
        (edges (log-one-stream db:eid&edge/props-stream 'db:eid&edge/props-stream db))
        (num-edges (length edges))
;        (_ (writeln `((event . found-concepts) (num . ,num-concepts))))
;        (_ (writeln `((event . found-edges) (num . ,num-edges))))
        (_ (log-one-countn db:cid->concept 'db:cid->concept db num-concepts))
        (_ (log-one-countn db:eid->edge 'db:eid->edge db num-edges))
        (edges (stream->list (db:subject->edge-stream db 1 #f #f #f)))
        (_ (writeln `((edges . ,edges))))
        (edges2 (append-map 
            (lambda (id) (stream->list (db:subject->edge-stream db id #f #f #f))) 
            (stream->list (in-range 0 num-edges))))                  ; TODO: num-edges or num-concepts?
        (_ (writeln `((db:subject->edge-stream . ,edges2))))
        (edges3 (append-map 
            (lambda (id) (stream->list (db:object->edge-stream db id #f #f #f))) 
            (stream->list (in-range 0 num-concepts))))
        (_ (writeln `((db:object->edge-stream . ,edges3))))
        (_ (log-one db:pmid->eid* 'db:pmid->eid* db 0))              ; empty I think - pmid, and no coverage
        (_ (log-one db:pmid&eid*-stream 'db:pmid&eid*-stream db))    ; empty I think - pmid, and no coverage
        (_ (log-one-countn db:subject->pids 'db:subject->pids db num-concepts))  ; TODO: num-edges?
        (_ (log-one-countn db:object->pids 'db:object->pids db num-concepts))
        ;(_ (log-one db:subject->pids 'db:subject->pids db "PR:000006537")) ; it does expect numbers, not strings
        ;(_ (log-one db:object->pids 'db:object->pids db "MONDO:0016147"))
        (_ (log-one db:synonym->cid* 'db:synonym->cid* db 0))  ; TODO: run a query with synonyms
        (_ (log-one db:xref->cid* 'db:xref->cid* db 0))        ; empty I think - xref
    )
        #f)
)
(define (run-all databases)
    (for ((db (map cdr (databases))))
        (run-db db)))
(run-all databases)
