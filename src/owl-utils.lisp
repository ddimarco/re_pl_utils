(in-package :re-pl-utils)

(defun pl-tree->string (tree)
  "translates an json-prolog:prolog answer into a more convenient form,
i.e. translates URIs from symbols to strings"
  (etypecase tree
    (list (mapcar #'pl-tree->string tree))
    (symbol (case tree
              (|'intersectionOf'| :intersection-of)
              (|'restriction'| :restriction)
              (|'someValuesFrom'| :some-values-from)

              ;; TODO: this is obviously incomplete
              (|'has_value'| :has-value)

              (otherwise (prolog->string tree))))))

(defun load-owl-file (filename)
  "instructs rosprolog to parse the given filename (expects a string)"
  (assert (not (search "~" filename)) nil
          "Please do not use tilde characters in path names: ~S" filename)
  (cut:with-vars-bound (?t)
      (cut:lazy-car
       (ll-assert-1 (json-prolog:prolog `("owl-parser:owl_parse" ,filename "false" "false" t)
                                        :package :re-pl-utils)))
    :package :re-pl-utils
    (prolog->string ?t)))

(defun load-local-owl-file (ros-package relative-dir fname)
  (load-owl-file (namestring (merge-pathnames
                               (make-pathname :directory
                                              `(:relative ,relative-dir)
                                              :name fname :type "owl")
                               (ros-load:ros-package-path ros-package)))))

(defmacro pl-wrap1var (pl-query)
  "helper macro for creating prolog-wrapper functions with 1 variable"
  (labels ((find-prefix-recursive (prefix tree)
             ;; looks for the variable name (which is prefixed with '?') in pl-query
             (etypecase tree
               (list (let ((result1 (find-prefix-recursive prefix (car tree))))
                       (if result1
                           result1
                           (find-prefix-recursive prefix (cdr tree)))))
               (string nil)
               (symbol (if (string= (subseq (symbol-name tree) 0 (length prefix)) prefix)
                           tree
                           nil)))))
    (let* ((pl-param (find-prefix-recursive "?" pl-query)))
      `(mapcar #'(lambda (x)
                   (cut:with-vars-bound (,pl-param) x :package :re-pl-utils
                     (pl-tree->string ,pl-param)))
               (cut:force-ll (json-prolog:prolog ,pl-query :package :re-pl-utils))))))

(defun get-superclasses (id &key (direct nil))
  (pl-wrap1var (if direct
                   `("owl_direct_subclass_of" ,id ?m)
                   `("owl_subclass_of" ,id ?m))))

(defun get-subclasses (id &key (direct nil))
  (remove id (pl-wrap1var (if direct
                              `("owl_direct_subclass_of" ?x ,id)
                              `("owl_subclass_of" ?x ,id)))))

(defun owl-class-properties (id)
  (mapcar #'(lambda (x)
              (cut:with-vars-bound (?p ?v) x :package :re-pl-utils
                (cons (pl-tree->string ?p) (pl-tree->string ?v))))
          (cut:force-ll (json-prolog:prolog `("class_properties" ,id ?p ?v)
                                            :package :re-pl-utils))))

(defun owl-has (id &key (role :subject))
  (mapcar #'(lambda (x)
              (cut:with-vars-bound (?p ?v) x :package :re-pl-utils
                (cons (pl-tree->string ?p) (pl-tree->string ?v))))
          (cut:force-ll (json-prolog:prolog
                         (ecase role
                           (:subject
                            `("owl_has" ,id ?p ?v))
                           (:predicate
                            `("owl_has" ?p ,id ?v))
                           (:object
                            `("owl_has" ?p ?v ,id)))
                         :package :re-pl-utils))))

(defmacro owl-has-query (&key subject predicate object (as-list t))
  (flet ((not-var-p (x)
           (or (not (symbolp x))
               (not (char= (elt (symbol-name x) 0) #\?)))))
    (let* ((qry (loop for x in (list subject predicate object)
                   for name in '(:subject :predicate :object)
                   collect
                     (if (null x)
                         (intern (format nil "?~a" (symbol-name name)))
                         x)))
           (vars (remove-if #'not-var-p qry))
           (result `(cut:force-ll (json-prolog:prolog
                                   ;; ugly: add comma in front of symbols
                                   `("owl_has"
                                     ,,@(loop for x in qry
                                            collect
                                            (if (and (symbolp x)
                                                     (not-var-p x))
                                                x
                                                ``,',x)))))))
      (if as-list
          `(mapcar #'(lambda (x)
                       (cut:with-vars-bound ,vars x
                         (list ,@(mapcar #'(lambda (x) `(pl-tree->string ,x)) vars))))
                   ,result)
          result))))

(defun get-individuals-of (id)
  (pl-wrap1var `("owl_individual_of" ?i ,id)))

(defun is-individual-of (indiv-id cls-id)
  (json-prolog:prolog `("owl_individual_of" ,indiv-id ,cls-id)))

(defun get-individual-superclasses (id)
  (remove-duplicates (pl-wrap1var `("owl_individual_of" ,id ?c)) :test #'string=))

(defun get-owl-restriction (id)
  (mapcar #'(lambda (x)
              (cut:with-vars-bound (?rtype ?r) x :package :re-pl-utils
                (cons ?rtype ?r)))
          (cut:force-ll (json-prolog:prolog `("owl_restriction" ,id ("restriction" ?rtype ?r))
                                            :package :re-pl-utils))))

(defun extract-properties (propid lst)
  (remove-if #'null (mapcar #'(lambda (pair)
                                (let ((sharp-pos (position #\# (car pair))))
                                  (if (string-equal propid (subseq (car pair) sharp-pos))
                                      (cdr pair))))
                            lst)))

(defun extract-literal-value (lst)
  "takes a list from extract-properties and finds the associated literals"
  ;;---TODO: check type and convert
  (loop for bdg in lst
     for type-lst = (cadr bdg)
     if (string= (car bdg) "literal")
     collect (car (last type-lst))))

;; e.g. (get-owl-class-properties "#dependsOnCapability" "http://www.roboearth.org/kb/roboearth.owl#Translation-LocationChange")
(defun get-owl-class-properties (propid id)
  (extract-properties propid (owl-class-properties id)))

(defun remove-rdf-ns (c)
  "removes everything before the '#' character in a string.
  E.g. http://www.owl-ontologies.com/Ontology1344505628.owl#Operator -> Operator"
  (if (find #\# c)
      (subseq c (1+ (position #\# c)))
      c))

(defun owl-classname->lisp (cls)
  "replaces CamelCase strings to be more lispy: OpenDoor -> open-door"
  (let ((result nil))
    (loop for c across cls
          for i from 0
          for last-char = (if result (elt cls (1- i)) nil)
          do
             (when (and (upper-case-p c)
                        (identity result)
                        (not (upper-case-p last-char)))
               (push #\- result))
             (push (if (char-equal c #\_)
                       #\-
                       (char-downcase c))
                   result))
    (coerce (reverse result) 'string)))

(defun has-superclass (clsid superclass)
  (json-prolog:prolog `("owl_subclass_of" ,clsid ,superclass)))

(defun get-all-has-value-props (id)
  (mapcar #'(lambda (x)
              (cut:with-vars-bound (?prop ?val) x :package :re-pl-utils
                (cons (pl-tree->string ?prop) (pl-tree->string ?val))))
          (cut:force-ll (json-prolog:prolog
                         `(and ("owl_subclass_of" ,id ?super)
                               ("owl_restriction" ?super ("restriction" ?prop ("has_value" ?val))))
                         :package :re-pl-utils))))

(defun has-individual-superclass (clsid supercls)
  (json-prolog:prolog `("owl_individual_of" ,clsid ,supercls)))

(defparameter *owl->lisp-hash* (make-hash-table :test 'equal))
(defparameter *lispy->owl-hash* (make-hash-table))

(defun owl-file-from-id (id)
  (subseq id (1+ (position #\/ id :from-end t)) (position #\# id)))

(defun remove-file-extension (fname-str)
  (subseq fname-str 0 (position #\. fname-str)))

(defun owl-id->lisp (id &key (package *package*) (pred-func-list nil))
  (check-type id string)
  (assert (string= (subseq id 0 7) "http://") (id) "~s does not look like an OWL identifier" id)
  ;; maybe add a suffix for the type? e.g. -object-prop, -class, ...
  (if (gethash id *owl->lisp-hash*)
      (gethash id *owl->lisp-hash*) ;; already exists in hashtable
      (let ((lispy-name (owl-classname->lisp (remove-rdf-ns id))))
        (let ((result
                (string->symbol
                 (loop for funcpair in pred-func-list
                       for predicate = (car funcpair)
                       for func = (cdr funcpair)
                       with result = lispy-name
                       when (funcall predicate id result)
                         do (setf result (funcall func id result))
                       finally (return result))
                 :package package)))
          (when (identity (gethash result *lispy->owl-hash*))
            (warn "Name conflict when converting ~a -> ~a (~a)"
                  id lispy-name (gethash result *lispy->owl-hash*))
            ;; append filename to resulting identifier
            (setf result (string->symbol (remove-file-extension (owl-file-from-id id))
                                         :package package)))
          (setf (gethash id *owl->lisp-hash*) result)
          (setf (gethash result *lispy->owl-hash*) id)
          result))))

(defun lisp->owl-id (lisp-symbol)
  (check-type lisp-symbol symbol)
  (gethash lisp-symbol *lispy->owl-hash*))

(defun clear-owl-id-hashes ()
  (clrhash *lispy->owl-hash*)
  (clrhash *owl->lisp-hash*))

(defun extract-individual-properties (prop proplist)
  ;; FIXME: we need to filter individuals out here
  (remove-if-not #'is-individual (extract-properties prop proplist)))

(defun is-owl-property (id)
  (json-prolog:prolog `("rdfs_class_property" ?x ,id)))

(defun is-individual (clsid)
  "exists an individual with name clsid?"
  (cut:with-vars-bound (?x)
      (cut:lazy-car (json-prolog:prolog
                     `("individual" ,clsid ?p ?x ?y) :package :re-pl-utils))
    :package :re-pl-utils
    (and (listp ?x)
         (> (length ?x) 1)
         ;; TODO: this is probably too strict
         (eq (car ?x) '|'http://www.w3.org/2002/07/owl#NamedIndividual'|))))


(defun rdf-type (owlid)
  (mapcar #'(lambda (x)
              (cut:with-vars-bound (?c) x :package :re-pl-utils
                (pl-tree->string ?c)))
          (cut:force-ll (json-prolog:prolog
                         `("rdf_has" ,owlid "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                                     ?c)
                         :package :re-pl-utils))))




