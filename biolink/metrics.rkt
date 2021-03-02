#lang racket
(provide (all-defined-out))

; configuration
(define num-to-skip 50000)

; state
(define _num-unify (box 0))
(define _num-unify* (box 0))
(define _num-mplus (box 0))
(define _num-bind (box 0))
(define _num-total (box 0))
(define _num-until-print (box num-to-skip))

; api
(define (get-counters)
    `(
        (unify . ,(unbox _num-unify))
        (unify* . ,(unbox _num-unify*))
        (mplus . ,(unbox _num-mplus))
        (bind . ,(unbox _num-bind))
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
    (set-box! _num-unify 0)
    (set-box! _num-unify* 0)
    (set-box! _num-mplus 0)
    (set-box! _num-bind 0)
    (set-box! _num-total 0)
)

