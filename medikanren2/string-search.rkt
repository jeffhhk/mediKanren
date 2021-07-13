#lang racket
(provide
 (prefix-out test: field-indexed)
 suffix:corpus->index
 suffix:corpus2->index
 vector-sparse-find
 test:read-name-corpus
 (prefix-out test: suffix:corpus->index-suffixes)
 test:suffix:corpus2->index-suffixes
 fn-cprop-primary
 (prefix-out test: build-name-string-search-v1)
 build-name-string-search-via-codec
 fn-concept-name-index
 (prefix-out test: foffs->concept)
 (prefix-out test: build-name-index-via-codec)
 (prefix-out test: ensure-name-index-built)
 name-string-matches
 find-concept-named
 string-search-init-rel
 test:verify-corpus-index
 make-stsopt
 stsopt-t
 )
(require racket/dict)
(require racket/vector)
(require "base.rkt")
(require "string-search-config.rkt")
(require "string-search-impl.rkt")
(require "string-search-impl2.rkt")

;;; *** routines for index building ***
(define (report-memory)
  (collect-garbage 'major)
  (printf "current-memory-use ~aMB\n" (exact->inexact (/ (current-memory-use) 1000000))))

(define (suffix:corpus->index-suffixes corpus)
  (foldl (lambda (i all)
           (foldl (lambda (j all) (cons (cons i j) all))
                  all (range (string-length (vector-ref corpus i)))))
         '() (range (vector-length corpus))))

(define (suffix:corpus2->index-suffixes hashcorpus)
  (for*/vector (((foffs s-searchable) (in-hash hashcorpus))
              (soffs (range (string-length s-searchable))))
    (suffix-key->bytes (cons foffs soffs))))

(define (suffix:corpus2->index hashcorpus)
  (define (suffix<? a b)    (suffix<?/corpus2 hashcorpus a b))
  (define suffixes (suffix:corpus2->index-suffixes hashcorpus))
  ;; It is acceptable to in-place sort the freshly allocated "suffixes",
  ;; but "corpus" must remain ordered by primary storage file offset in
  ;; order for vector-sparse-find to correctly guide the search.
  (report-memory)
  (printf "sorting suffixes\n")
  (vector-sort! suffixes suffix<?)
  (printf "sorting complete\n")
  suffixes)

(define (test:suffix:corpus2->index-suffixes veccorpus)
  (define hashcorpus (in-pairs->hash veccorpus))
  (define (suffix<? a b)    (suffix<?/corpus2 hashcorpus a b))
  (suffix:corpus2->index-suffixes hashcorpus))

(define (output/filename fname absdOut out->)
  (printf "writing ~s\n" fname)
  (time (call-with-output-file (expand-user-path (build-path absdOut fname)) out->)))

(define (build-name-string-search-v1 concept* absdOut)
  (printf "building name search corpus...\n")
  (define name-corpus
    (time (vector-map (lambda (c) (let ((name (concept-name c)))
                                    (if name (string/searchable name) "")))
                      concept*)))
  (printf "building name search index...\n")
  (define name-index (time (suffix:corpus->index name-corpus)))
  (printf "indexed ~a suffixes\n" (vector-length name-index))
  (output/filename
   fn-concept-name-corpus
   absdOut
    (lambda (out) (for ((s (in-vector name-corpus))) (write-scm out s))))
  (output/filename
   fn-concept-name-index
   absdOut
    (lambda (out) (write-suffix-keys out name-index))))

;;; in-name-corpus
;;;   Opens a sequence produced by decoding a primary index file.  Auto-closes upon encountering eof.
(define (in-name-corpus fname absdOut)
  (define fdIn (open-input-file (expand-user-path (build-path absdOut fname)) #:mode 'binary))
  (define (read-decode fdIn)
    (if (port-closed? fdIn)
        eof
        (if (eof-object? (peek-byte fdIn))
            (begin
              (close-input-port fdIn)
              eof)
            (let* ((foffs (file-position fdIn))
                   (v (decode fdIn schema-pri)))
              (match (name-ornull-from-pri v)
                (`(,name)
                 (let ((s-searchable (string/searchable name)))
                   (cons s-searchable foffs)))
                (_ (read-decode fdIn)))))))
  (in-port read-decode fdIn))

(define (in-pairs->hash in-pairs)
  (let ((h (make-hasheqv))) ; TODO perf?
    (for ((kv in-pairs))
      (match kv
        ((cons s-searchable foffs)
          (hash-set! h foffs s-searchable))))
    h))

(define (read-name-corpus fname absdOut)
  (let ((in-corpus (in-name-corpus fname absdOut)))
    (in-pairs->hash in-corpus)))

;; For testing v1, we need vector-find-sparse, which needs a list of pairs ordered by foffs
(define (test:read-name-corpus fname absdOut)
  (let ((in-corpus (in-name-corpus fname absdOut)))
    (for*/vector ((kv in-corpus))
      kv)))


;;; build-name-index-via-codec
;; Name+id corpus must live on disk so as to possess file offsets.
(define (build-name-index-via-codec fname absdOut)
  (let ((hashcorpus (read-name-corpus fname absdOut)))
    (report-memory)
    (printf "building name search index...\n")
    (values hashcorpus (suffix:corpus2->index hashcorpus))))

(define (build-name-string-search-via-codec fn-pri fn-index absdOut)
  (define-values (hashcorpus name-index) (build-name-index-via-codec fn-pri absdOut))
  (printf "indexed ~a suffixes, now writing...\n" (vector-length name-index))
  (call-with-atomic-output-file
    (expand-user-path (build-path absdOut fn-index))
    (lambda (fd-index adir-junk)
      (for* ((ni name-index))
          (write-bytes ni fd-index)
          ))))

(define (ensure-name-index-built absd-index fn-concept-name-index)
  (let* (
         (absf-index (path->string (simplify-path (build-path absd-index fn-concept-name-index)))))
    (printf "checking for index ~a\n" absf-index)
    (unless (file-exists? absf-index)
      (build-name-string-search-via-codec fn-cprop-primary fn-concept-name-index absd-index))))

(define (assert-name-index-built absd-index fn-concept-name-index)
  (let* (
         (absf-index (path->string (simplify-path (build-path absd-index fn-concept-name-index)))))
    (printf "checking for index ~a\n" absf-index)
    (unless (file-exists? absf-index)
      (error (format "Use of string search requires index preparation.  Call string-search-init-rel: ~a" absf-index)))))

(define (test:verify-corpus-index hashcorpus index)
  (define (<=? c d) (not (string<? d c)))
  (define (shorten s) (substring s 0 (min (string-length s) 20)))
  (for ((i (range 1 (vector-length index))))
    (let* (
        (skey-prev (bytes->suffix-key (vector-ref index (- i 1)) 0))
        (skey (bytes->suffix-key (vector-ref index i) 0))
        (ss-prev (substring (hash-ref hashcorpus (car skey-prev)) (cdr skey-prev)))
        (ss (substring (hash-ref hashcorpus (car skey)) (cdr skey))))
      (unless (<=? ss-prev ss)
        (error (format "index out of sequence: ~a should be < ~a\n" (shorten ss-prev) (shorten ss)))))
    ))

(define (name-string-matches rel substrings (stsopt stsopt-default))
  (unless (andmap string? substrings)
    (error "name-string-matches: substrings must be a list of strings"))
  (define absd-index (hash-ref (relation-definition-info rel) 'path))
  (assert-name-index-built absd-index fn-concept-name-index)
    (let* ((pris (db:~name*->concept*/options2 stsopt absd-index fn-cprop-primary fn-concept-name-index substrings)))
      (map uri-from-pri pris)))

(define (find-concept-named rel substrings (stsopt stsopt-default))
  (let ((uris (name-string-matches rel substrings stsopt))) ;; ("HGNC:01" "HGNC:02")
    (define-relation/table (found xxx)
      'source-stream (map list uris))                ;; (("HGNC:01") ("HGNC:02"))
    (define-relation (found2 id subj object)
      (found id)                                     ;; logical AND, as in conde
      (rel id subj object))
    found2))

;;; Prepare a string search index, if it has not already been prepared.
;; If the the string search index has already been prepared, string-search-init-rel
;; will exit quickly.  If no string search index is prepared, calls
;; to name-string-matches and find-concept-named will fail.
(define (string-search-init-rel rel)
  (let* ((reld-index (hash-ref (relation-definition-info rel) 'path)))
    (ensure-name-index-built reld-index fn-concept-name-index)
  ))
