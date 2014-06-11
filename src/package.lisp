(defpackage re-pl-utils
  (:nicknames re-pl-utils)
  (:use #:cpl #:roslisp)

  (:export #:pl-query #:pl-matrix->pose #:owl-matrix->transform
           #:prolog->string
           #:ll-assert-1 #:string->symbol

           ;; owl-utils
           #:pl-tree->string #:load-local-owl-file #:load-owl-file
           #:get-superclasses #:get-subclasses
           #:owl-has #:owl-has-query
           #:get-owl-restriction #:has-superclass
           #:get-individuals-of #:is-individual-of
           #:get-individual-superclasses
           #:extract-properties #:owl-class-properties
           #:extract-literal-value
           #:rdf-type
           #:get-all-has-value-props
           #:get-owl-class-properties
           #:has-individual-superclass

           #:remove-rdf-ns
           #:owl-id->lisp #:lisp->owl-id

           #:extract-individual-properties
           ;;---TODO: rename to owl-class-p etc
           #:is-owl-property #:is-individual

           #:has-transformation-matrix
           #:owl-matrix->pose

           ;; knowrob-utils
           #:get-recipe-subactions
           #:direct-subparts #:connected-to
           #:defined-in-map #:individuals-of-class-in-map
           #:re-download-models-for

           ;; srdl-utils
           #:resolve-relative-transformation-matrix
           #:srdl-component-relative-pose
           #:srdl-component-urdf-links
           #:srdl-component-urdf-name
           #:srdl-components)
  )
