#lang racket
(provide
  (all-from-out "common.rkt")
  (all-defined-out))
(require "common.rkt")

  (define (query1)
    (run* (c) (categoryo c)))

  (define (query2)
    (run* (p) (predicateo p)))

  (define (query3)
  (run 10 (c) (concepto c))
  )

  (define (query4)
  (run 10 (e) (edgeo e))
  )

  (define (query5)
  (run* (c) (~name*-concepto (list "imatin" "400") c))
  )

  (define (query6)
  (run* (c)
    (~cui*-concepto
      ;; Match CURIEs that match any of the listed fragments.
      (list "UMLS:C0004096" "DOID:2841" "HP:0002099" "MONDO:0004979")
      c))
  )

  (define (query7)
  (run* (xref c)
    (membero xref '("DOID:2841" "HP:0002099" "MONDO:0004979"))
    (xref-concepto xref c))
  )

  (define (query8)
  (run 3 (pmid e) (pmid-edgeo pmid      e))
  )

  (define (query9 v)
  (run*       (e) (pmid-edgeo v e))
  )

(module+ main
(displayln "all categories:")
(pretty-print (query1))

(newline)
(displayln "all predicates:")
(pretty-print (query2))

(newline)
(displayln "some concepts:")
(pretty-print (query3))

(newline)
(displayln "some edges:")
(pretty-print (query4))

(newline)
(displayln "fuzzy name search:")
(time (pretty-print
        ;; Match names that include all listed fragments.
        (query5)))

(newline)
(displayln "CURIE search:")
(time (pretty-print
        (query6)))

(newline)
(displayln "xref search:")
(time (pretty-print
        (query7)))

(newline)
(displayln "finding pubmed ids and associated edges:")
(time (pretty-print (query8)))
(displayln "associating specific pubmed ids with edges:")
(time (pretty-print (query9 "1000085")))
(time (pretty-print (query9 "1000018")))
(time (pretty-print (query9 "10000")))
(time (pretty-print (query9 "999999")))
)
