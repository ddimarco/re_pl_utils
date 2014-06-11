(in-package :re-pl-utils)

(defun resolve-relative-transformation-matrix (matrix-id)
  (labels ((find-relative-matrix (m &optional (prev nil))
             (let ((rel
                     (cut:force-ll
                      (re-pl-utils:pl-query (?x)
                          `("owl_has" ,m "http://ias.cs.tum.edu/kb/knowrob.owl#relativeTo" ?x)
                        (re-pl-utils:pl-tree->string ?x)))))
               (if rel
                   (find-relative-matrix (car rel) (append (list m) prev))
                   (append (list m) prev)))))
    (let ((kinematic-chain (find-relative-matrix matrix-id)))
      ;; TODO: check order
      (apply #'cl-transforms:transform*
       (mapcar #'re-pl-utils:owl-matrix->transform kinematic-chain)))))

(defun srdl-component-urdf-links (robotid &optional
                                            (component-type
                                             "http://ias.cs.tum.edu/kb/srdl2-comp.owl#Camera"))
  (cut:lazy-mapcar #'(lambda (x)
                       (cut:with-vars-bound (?link) x :package :re-pl-utils
                         (pl-tree->string ?link)))
                   (json-prolog:prolog
                    `(and ("srdl2:sub_component" ,robotid ?x)
                          ("owl_individual_of" ?x ?c)
                          ("owl_subclass_of" ?c ,component-type)
                          ("owl_has" ?x "http://ias.cs.tum.edu/kb/srdl2-comp.owl#urdfName"
                                     ("literal" ?link)))
                    :package :re-pl-utils)))

(defun srdl-component-urdf-name (srdl-component)
  (re-pl-utils:pl-query (?link)
      `("owl_has" ,srdl-component "http://ias.cs.tum.edu/kb/srdl2-comp.owl#urdfName"
                  ("literal" ?link))
      ?link))

(defun srdl-components (robotid &optional
                                  (component-type
                                   "http://ias.cs.tum.edu/kb/srdl2-comp.owl#Camera"))
  (cut:lazy-mapcar #'pl-tree->string
          (pl-query (?x)
              `(and ("srdl2:sub_component" ,robotid  ?x)
                    ("owl_individual_of" ?x ?c)
                    ("owl_subclass_of" ?c ,component-type))
            ?x)))
