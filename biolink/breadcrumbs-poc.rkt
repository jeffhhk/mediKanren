#lang racket/base
(require racket/set racket/string racket/pretty racket/match)

; POC for capturing breadcrumbs from a query/graph
;
; Here are three use cases related to query reproducibility:
;
;   use case 1:
;     - Run a previously run query.  Previously run query left behind a "breadcrumb" file 
;       showing how the query ran.  If query results are less complete than on previous runs,
;       emit human-readable warning information about why the query might now be incomplete
;       or missing.  For example, if a certain database was previously loaded and supported
;       the previous query result, report the name of the previously supporting database
;       that is not currently loaded.
;
;   use case 2:
;     - Create mock databases for running unit tests, possibly involving human effort,
;       able to exercise the same or very similar code paths to a full set of databases.
;
;   use case 3:
;     - Run Q against D1 to yield result R and save a small database D2 capable of supporting
;       the same query result.
;
; This program implements use case 3, which makes use case 2 very easy, and drastically
; decreases the scope of use case 1.

;
; 1) run a query/graph to produce res1
; 2) (write-breadcrumbs (breadcrumbs1 res1) "data/edges.input.scm")
; 3) run indexing scripts as follows:
;   rm -f biolink/data/repro1/*.csv && (cd biolink && racket -l errortrace -u database-from-values.rkt data repro1 && racket csv-graph-to-db.rkt data repro1 && racket build-string-index.rkt data repro1)
; The directory biolink/data/repro1 will now contain a minimal database for reproducing res1 from query.
; 4) Confirm that the query can be reproduced by launching racket with only the repro1 database.  E.g.:
;   time (cd biolink && env mk_databases=repro1 racket)

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

(define (write-breadcrumbs bcs fnout)
    (define fout (open-output-file fnout))
    (for ((bc bcs))
        (writeln bc fout))
    (close-output-port fout))

; *** BEGIN MODIED FORK OF edges-to-csv.rkt ***
(define argv (current-command-line-arguments))
(define argv-expected '#(DATA_DIR GRAPH_DIR))
(when (not (= (vector-length argv-expected) (vector-length argv)))
  (error "command line argument mismatch:" argv-expected argv))

(define data-dir  (vector-ref argv 0))
(define graph-dir (vector-ref argv 1))
(define (graph-path fname)
  (expand-user-path (build-path data-dir graph-dir fname)))
(define (graph-path/gname fname) (graph-path (string-append graph-dir fname)))

(define (call/files fins fouts p)
  (let loop ((fins fins) (ins '()))
    (if (null? fins)
      (let loop ((fouts fouts) (outs '()))
        (if (null? fouts)
          (apply p (append (reverse ins) (reverse outs)))
          (call-with-output-file
            (car fouts) (lambda (out) (loop (cdr fouts) (cons out outs))))))
      (call-with-input-file
        (car fins) (lambda (in) (loop (cdr fins) (cons in ins)))))))

(define-syntax-rule (let/files ((in fin) ...) ((out fout) ...) body ...)
  (call/files (list fin ...) (list fout ...)
              (lambda (in ... out ...) body ...)))

(define (csv-escape s)
  (string-append "\"" (string-replace s "\"" "\"\"") "\""))

(let/files ((in-edges (graph-path "edges.input.scm")))
  ((out-node     (graph-path/gname ".node.csv"))
   (out-nodeprop (graph-path/gname ".nodeprop.csv"))
   (out-edge     (graph-path/gname ".edge.csv"))
   (out-edgeprop (graph-path/gname ".edgeprop.csv")))
  (fprintf out-node     ":ID\n")
  (fprintf out-nodeprop ":ID,propname,value\n")
  (fprintf out-edge     ":ID,:START,:END\n")
  (fprintf out-edgeprop ":ID,propname,value\n")
  (let loop ((i 0) (concepts (set)))
    (define (add-concept concepts c)
      (define curie    (cadr    c))
      (define name     (caddr   c))
      (define category (cdr (cadddr  c)))
      (define cprops   (cadddr (cdr c)))
      (cond ((set-member? concepts curie) concepts)
            (else (fprintf out-node "~a\n" curie)
                  (fprintf out-nodeprop "~a,~a,~a\n" curie "name"
                           (csv-escape name))
                  (fprintf out-nodeprop "~a,~a,~a\n" curie "category"
                           (csv-escape category))
                  (for-each (lambda (kv)
                              (fprintf out-nodeprop "~a,~a,~a\n" curie
                                       (car kv) (csv-escape (cdr kv))))
                            cprops)
                  (set-add concepts curie))))
    (define e0 (read in-edges))
    (unless (eof-object? e0)
      (define e (cddr e0))
      (define subject   (car    e))
      (define object    (cadr   e))
      (define predicate (caddr  e))
      (define eprops    (cadddr e))
      (fprintf out-edge "~a,~a,~a\n" i (cadr subject) (cadr object))
      (fprintf out-edgeprop "~a,~a,~a\n" i "edge_label" (cdr predicate))
      (for-each (lambda (kv) (fprintf out-edgeprop "~a,~a,~a\n" i (car kv)
                                      (csv-escape (cdr kv))))
                eprops)
      (loop (+ i 1) (add-concept (add-concept concepts subject) object)))))

; *** END MODIFIED FORK OF edges-to-csv.rkt ***
