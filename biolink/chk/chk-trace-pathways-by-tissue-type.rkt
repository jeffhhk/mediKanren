#lang racket
(require chk)
(require "../pieces-parts/trace-pathways-by-tissue-type.rkt")
(chk
    ; the first example in comments of trace-pathways-by-tissue-type.rkt
    #:do (displayln (count-by-cell-expressed-in ards-pos-reg))
    #:t #f)

