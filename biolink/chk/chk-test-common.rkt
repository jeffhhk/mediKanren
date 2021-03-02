#lang racket

; run as:
;   raco pkg install --batch --deps search-auto chk cover
;   cd biolink
;   raco cover --verbose ./repr.rkt ./string-search.rkt ./db.rkt ./mk.rkt ./mk-db.rkt ./common.rkt ./test-common.rkt chk1/main.rkt

(require chk)
(require "../test-common.rkt")

; A chk #:do will always be run, but to be counted as one "test" in
; the summary, appending a #:t #t is required.
;
; In the chk docs, the authors state a strong opinion that running code without
; making an assertion is not useful.  I respectfully disagree.

(chk
    #:do (run1)
    #:t #t)

(chk
    #:do (run2)
    #:t #t)
