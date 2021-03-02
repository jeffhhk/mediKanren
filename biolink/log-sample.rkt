#lang racket
(provide 
    logsamp-sample
    the-logsamp
)

; A log writer, limited to display an event only when an average time of
; dt-expected has elapsed.
(struct evsamp (
    evid
    (t-next #:mutable)
    dt-expected))

(define (calc-t-next dt-expected t)
    (let* (
        (dt (* dt-expected (+ 0.5 (random))))
        )
        (+ t dt)))

(define (make-evsamp evid dt-expected)
    (let* (
        (t (current-inexact-milliseconds))
        (t-next (calc-t-next dt-expected t))
        )
        (evsamp evid t-next dt-expected)
    ))

(define (evsamp-sample evsamp x)
    (let* (
            (t (current-inexact-milliseconds))
            (dt-remaining (- (evsamp-t-next evsamp) t))
            )
        (if (> dt-remaining 0.0)
            x
            (begin
                (set-evsamp-t-next! evsamp (calc-t-next (evsamp-dt-expected evsamp) t))
                (displayln `((event . ,(string-append "logsamp_" (evsamp-evid evsamp))) (x . ,x)))
                x
            ))))

; A collection of log writers organized by key evid.
(struct logsamp (evsamp-from-evid))

(define (make-logsamp)
    (logsamp (make-hash)))

(define (ensure-evsamp logsamp evid dt)
    (let* ((evsamp-from-evid (logsamp-evsamp-from-evid logsamp)))
        (hash-ref! evsamp-from-evid evid (lambda () (make-evsamp evid dt)))
    ))

; *** Begin provided API ***

; Identify an stream with evid, and log them only when an average of
; dt has elapsed.
(define (logsamp-sample 
        logsamp    ; instance
        evid       ; event type
        dt         ; minimum average time (in ms) between events of the same evid
        x          ; datum to sample
        )
    (let* ((evsamp (ensure-evsamp logsamp evid dt)))
        (evsamp-sample evsamp x)
    ))

; TODO?: use racket singleton
(define the-logsamp (make-logsamp))

; *** End provided API ***
