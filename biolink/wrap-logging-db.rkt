#lang racket
(provide
    wrap-logging-db)
(define (wrap-logging-db fnout-log db)
  (match db
    ((vector category* predicate*
          (cons cui-corpus cui-index)
          (cons name-corpus name-index)
          catid->cid* cid->concept eid->edge
          cid&concept-stream eid&edge/props-stream
          subject->edge-stream object->edge-stream
          pmid->eid* pmid&eid*-stream subject->pids object->pids
          synonym->cid* xref->cid*)
      (let* (
        (fout (open-output-file fnout-log #:mode 'text #:exists 'append))
        (write-dblog (lambda (x) (writeln x)))
        (writefn-dblog (lambda (tag fn)
          (lambda (db . args2)
            (let ((res (apply fn (cons db args2))))
              (if (not (null? args2))
                (write-dblog `(
                  (event . ,tag)
                  (args . (,args2))
                  (res . (,
                    (if (and (stream? res) (not (stream-empty? res))) 
                      (stream-first res)
                      '())))
                  ))
                #f)
              res))))
        (catid->cid* (writefn-dblog 'catid->cid* catid->cid*))
        (cid->concept (writefn-dblog 'cid->concept cid->concept))
        (eid->edge (writefn-dblog 'eid->edge eid->edge))
        (subject->edge-stream (writefn-dblog 'subject->edge-stream subject->edge-stream))
        (object->edge-stream (writefn-dblog 'object->edge-stream object->edge-stream))
        (pmid->eid* (writefn-dblog 'pmid->eid* pmid->eid*))
        (subject->pids (writefn-dblog 'subject->pids subject->pids))
        (object->pids (writefn-dblog 'object->pids object->pids))
        (synonym->cid* (writefn-dblog 'synonym->cid* synonym->cid*))
        (xref->cid* (writefn-dblog 'xref->cid* xref->cid*))
        )
        (begin
          ; (write-dblog `(
          ;   (event . static-data)
          ;   (category* . ,category*)
          ;   (predicate* . ,predicate*)
          ;   (cui . ,(cons cui-corpus cui-index))
          ;   (name . ,(cons name-corpus name-index))
          ;   (cid&concept-stream . ,cid&concept-stream)
          ;   (eid&edge/props-stream . ,eid&edge/props-stream)
          ;   (pmid&eid*-stream . ,pmid&eid*-stream)
          ;   ))
          (vector category* predicate*
                    (cons cui-corpus cui-index)
                    (cons name-corpus name-index)
                    catid->cid* cid->concept eid->edge
                    cid&concept-stream eid&edge/props-stream
                    subject->edge-stream object->edge-stream
                    pmid->eid* pmid&eid*-stream subject->pids object->pids
                    synonym->cid* xref->cid*)
                )))
    (_ (raise "could not destructure db"))
    ))
