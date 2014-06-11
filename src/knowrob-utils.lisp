(in-package :re-pl-utils)

(defun get-recipe-subactions (recipe)
  (mapcar #'prolog->string
          (cut:with-vars-bound (?s)
              (cut:lazy-car (ll-assert-1 (json-prolog:prolog `("plan_subevents" ,recipe ?s)
                                                             :package :re-pl-utils)))
            :package :re-pl-utils
            ?s)))

(defun defined-in-map (mapid)
  (mapcar #'(lambda (x)
              (cut:with-vars-bound (?indiv) x :package :re-pl-utils (pl-tree->string ?indiv)))
          (cut:force-ll (json-prolog:prolog
                         `("owl_has" ?indiv "http://ias.cs.tum.edu/kb/knowrob.owl#describedInMap" ,mapid)
                         :package :re-pl-utils))))

(defun individuals-of-class-in-map (supercls map)
  (remove-duplicates
   (mapcar #'(lambda (x)
               (cut:with-vars-bound (?indiv) x :package :re-pl-utils
                 (pl-tree->string ?indiv)))
           (cut:force-ll (json-prolog:prolog
                          `(and ("owl_has" ?indiv
                                           "http://ias.cs.tum.edu/kb/knowrob.owl#describedInMap" ,map)
                                ("owl_individual_of" ?indiv ,supercls))
                          :package :re-pl-utils)))
   :test 'string=))
