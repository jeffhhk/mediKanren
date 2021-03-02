#lang racket

; run as:
;   raco pkg install --batch --deps search-auto chk cover
;   cd biolink
;   raco test ./chk1/main2.rkt
;   raco cover --verbose ./repr.rkt ./string-search.rkt ./db.rkt ./mk.rkt ./mk-db.rkt ./common.rkt ./test-common.rkt chk1/main2.rkt

(require chk)
(require "../common.rkt")
(require "../example-common.rkt")

; A chk #:do will always be run, but to be counted as one "test" in
; the summary, appending a #:t #t is required.
;
; In the chk docs, the authors state a strong opinion that running code without
; making an assertion is not useful.  I respectfully disagree.

(chk
    #:do (query1)
    #:t #t)

(chk
    #:do (query2)
    #:t #t)

(chk
    #:do (query3)
    #:t #t)

(chk
    #:do (query4)
    #:t #t)

(chk
    #:do (query5)
    #:t #t)

(chk
    #:do (query6)
    #:t #t)

(chk
    #:do (query7)
    #:t #t)

(chk
    #:do (query8)
    #:t #t)

(chk
    #:do (query9 "1000085")
    #:t #t)

(chk
    #:do (query9 "1000018")
    #:t #t)

(chk
    #:do (query9 "10000")
    #:t #t)

(chk
    #:do (query9 "999999")
    #:t #t)
