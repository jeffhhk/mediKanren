#lang racket

(define (every-other xs)
  (define (iter xs zs)
    (match xs
      ((cons y1 (cons z1 xstail)) (iter xstail (cons z1 zs)))
      (_ (reverse zs))))
  (iter xs '()))

;(every-other '(1 2 3 4 5))
;'(2 4)

(define (breadcrumbs1 query-graph-result)
    (let* (
            (from-edge (lambda (edge)
                (cdr ((cdr (assoc edge (cdr query-graph-result))) 'force))))
            (edges (append-map every-other (car query-graph-result))))
        (append-map from-edge edges)))

(define (write-breadcrumbs bcs reldir)
    (if (not (directory-exists? reldir))
      (make-directory reldir)
      #f)
    (define relfile (build-path reldir "edges.input.scm"))
    (define fout (open-output-file relfile))
    (for ((bc bcs))
        (writeln bc fout))
    (close-output-port fout))
