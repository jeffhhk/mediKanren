#lang racket
(provide (all-defined-out))
(require "../../pieces-parts/query.rkt"
         racket/engine)

;; JH: breadcrumb extracted:
;;   (write-breadcrumbs (breadcrumbs1 (qA)) "data/breadcrumb4a")
;;   (write-breadcrumbs (breadcrumbs1 (qB)) "data/breadcrumb4b")

(define crosskg-mondo-disease-curies
  '("MONDO:0016192"
    "DOID:0050890"
    "MONDO:0016153"
    "MONDO:0016147"
    "MONDO:0016191"
    "MONDO:0016196"
    "MONDO:0016187"))

(define (qA) (query/graph
          ((Drug #f)
           (PR #f)
           (Disease "MONDO:0016147")
           )
          ((Drug->PR (list "biolink:negatively_regulates_entity_to_entity") #;(edge/db? 'textminingprovider))
           (PR->Disease
            (list
             "gene_mapped_to_disease"
             "gene_involved_in_pathogenesis_of_disease"
             "gene_associated_with_disease"
             "associated_with_disease"
             "INVERTED:disease_has_basis_in_dysfunction_of") #;(edge/db? 'rtx2_2020_09_16)))
          (Drug Drug->PR PR)
          (PR PR->Disease Disease)))

(define (CHEBI--negatively-regulates-->PR-->MONDO)
  (curies/query
   (time (qA))
   'Drug))


(define (qB)
  (query/graph
            ((Drug #f)
            (PR #f)
            (Disease "MONDO:0000510"))
            ((Drug->PR (list "biolink:positively_regulates_entity_to_entity") (edge/db? 'textminingprovider)) 
            (PR->Disease
              (list
              "gene_mapped_to_disease"
              "gene_involved_in_pathogenesis_of_disease"
              "gene_associated_with_disease"
              "associated_with_disease"
              "INVERTED:disease_has_basis_in_dysfunction_of") (edge/db? 'rtx2_2020_09_16)))
            (Drug Drug->PR PR)
            (PR PR->Disease Disease))
)

(define (CHEBI--postively-regulates-->PR-->MONDO)
  (curies/query
   (time (qB))
   'Drug))

(module+ main
  (displayln (format "CHEBI--negatively-regulates-->PR-->MONDO=~a" (CHEBI--negatively-regulates-->PR-->MONDO)))
  (displayln (format "CHEBI--postively-regulates-->PR-->MONDO=~a" (CHEBI--postively-regulates-->PR-->MONDO)))
)
