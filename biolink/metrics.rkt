#lang racket
(provide (all-defined-out))

; configuration
(define num-to-skip 50000)

; state
(define _num-mk-unify (box 0))
(define _num-mk-unify* (box 0))
(define _num-mk-mplus (box 0))
(define _num-mk-bind (box 0))
(define _num-total (box 0))
(define _num-until-print (box num-to-skip))

; api
(define (get-counters)
    `(
        (mk-unify . ,(unbox _num-mk-unify))
        (mk-unify* . ,(unbox _num-mk-unify*))
        (mk-mplus . ,(unbox _num-mk-mplus))
        (mk-bind . ,(unbox _num-mk-bind))
        (total . ,(unbox _num-total))
    ))

(define (check-emit)
    (set-box! _num-total (+ (unbox _num-total) 1))
    (set-box! _num-until-print (- (unbox _num-until-print) 1))
    (if (<= (unbox _num-until-print) 0)
        (begin
            (set-box! _num-until-print num-to-skip)
            (displayln (get-counters))
        )
        #f))

(define (incr-counter ctr)
    (set-box! ctr (+ (unbox ctr) 1))
    (check-emit))

(define (reset-counters)
    (set-box! _num-mk-unify 0)
    (set-box! _num-mk-unify* 0)
    (set-box! _num-mk-mplus 0)
    (set-box! _num-mk-bind 0)
    (set-box! _num-total 0)
)

