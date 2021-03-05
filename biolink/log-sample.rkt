#lang racket
(provide 
    logsamp-sample
    logsamp-reset
    the-logsamp
)

; A log writer, limited to display an event only when an average time of
; dt-expected has elapsed.
(struct evsamp (
    evid
    (num-to-wait #:mutable)
    (num-to-wait-prev #:mutable)
    (t-prev #:mutable)
    (num-total #:mutable)
    dt-expected
    (is-warmup #:mutable)))

(define (calc-num-to-wait evsamp t)
    (let* (
            (dt (- t (evsamp-t-prev evsamp)))
            (rate (/ (evsamp-num-to-wait-prev evsamp) (+ dt 0.001)))
            (r (+ 0.5 (random)))
            (num-to-wait (* r rate (evsamp-dt-expected evsamp))))
;        (displayln (format "t=~a dt=~a r=~a num-to-wait=~a" t dt r num-to-wait))
        (exact-round num-to-wait)))

(define (make-evsamp evid dt-expected num-warmup)
    (evsamp evid num-warmup num-warmup (current-inexact-milliseconds) 0 dt-expected #t)
    )

(define (emit-sample evsamp x)
    (displayln `(
        (event . ,(string-append "logsamp_" (evsamp-evid evsamp))) 
        (n . ,(evsamp-num-total evsamp)) 
        (x . ,x))))

(define (evsamp-sample evsamp x)
    (set-evsamp-num-total! evsamp (+ 1 (evsamp-num-total evsamp)))
    (if (> (evsamp-num-to-wait evsamp) 0)
        (begin
            (set-evsamp-num-to-wait! evsamp (- (evsamp-num-to-wait evsamp) 1))
            (if (evsamp-is-warmup evsamp)
                (emit-sample evsamp x)
                #f)
            x
        )
        (let* (
                (t (current-inexact-milliseconds))
                ; current-inexact-milliseconds is the most expensive operation.
                ; Compared with asking for the time on every evsamp-sample, 
                ; rearranging our computation to only ask for the time when
                ; we have decided to emit a message is a performance increase
                ; of around 11x.
                ;
                ; use case measured:
                ;   db.rkt:
                ;     (for/vector ((x name-index)) (logsamp-sample the-logsamp "name-index-item" 2000 x))
                ;   main.rkt:
                ;     (require "common.rkt")
                ;     (load-databases #t)
                ;   configure to load covid19 database
                (num-to-wait (calc-num-to-wait evsamp t)))
            (begin
                (set-evsamp-num-to-wait! evsamp num-to-wait)
                (set-evsamp-num-to-wait-prev! evsamp num-to-wait)
                (set-evsamp-t-prev! evsamp t)
                (emit-sample evsamp x)
                (if (evsamp-is-warmup evsamp)
                    (set-evsamp-is-warmup! evsamp #f)
                    #f)
                x
            ))))

; A collection of log writers organized by key evid.
(struct logsamp (
    (evsamp-from-evid #:mutable)
))

(define (make-logsamp)
    (logsamp (make-hash)))

(define (logsamp-reset-impl logsamp)
    (set-logsamp-evsamp-from-evid! (make-hash)))

(define (ensure-evsamp logsamp evid dt num-warmup)
    (let* ((evsamp-from-evid (logsamp-evsamp-from-evid logsamp)))
        (hash-ref! evsamp-from-evid evid (lambda () (make-evsamp evid dt num-warmup)))
    ))

; *** Begin provided API ***

; Identify an stream with evid, and log them only when an average of
; dt has elapsed.
(define (logsamp-sample 
        logsamp    ; instance
        evid       ; event type
        dt         ; minimum average time (in ms) between events of the same evid
        #:num-warmup (num-warmup 1) ; number of samples to display before sampling
        x          ; datum to sample
        )
    (let* ((evsamp (ensure-evsamp logsamp evid dt num-warmup)))
        (evsamp-sample evsamp x)
    ))

(define (logsamp-reset logsamp)
    (logsamp-reset-impl logsamp))

; TODO?: use racket singleton
(define the-logsamp (make-logsamp))

; *** End provided API ***