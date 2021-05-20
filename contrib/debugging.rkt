#lang racket
(require
    racket/dict
    )
(provide
 log-return
 )

;;;; "contrib/debugging.rkt"
;;
;; Contributed functions useful for debugging with medikanren, but not specific
;; to medikanren version.


;;; Observe the value returned from a function, and how long it took to run.
;; Optionally, join decorate the return value with extra information using "decorate".
(define (log-return props thunk-run #:decorate [decorate (lambda (x) x)])
  (define (finish props v dt)
    (let* (
           (ev (assoc 'event props))
           (ev (if ev (cdr ev) "unknown")))
      (writeln (dict-set* props 'event ev 'val v 'dtms dt))))
  (let* ((t0 (current-inexact-milliseconds)))
    (with-handlers
      ((exn:fail? (lambda (ex)
                    (finish props (format "(EXCEPTION ~s)" ex) (- (current-inexact-milliseconds) t0))
                    (raise ex))))
      (let* ((v (thunk-run))
             (dt (- (current-inexact-milliseconds) t0))
             (v-decorated (decorate v)))
        (finish props v-decorated dt)
        v))))
