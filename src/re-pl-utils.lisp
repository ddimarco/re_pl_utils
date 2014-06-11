(in-package re-pl-utils)

(setf json-prolog:*service-namespace* "/knowrob")

(defmacro pl-query (vars query &body body)
  `(cut:lazy-mapcar #'(lambda (bdg)
                        (cut:with-vars-bound ,vars
                            bdg
                          ,@body))
                    (json-prolog:prolog ,query :package ,*package*)))

(defun pl-matrix->pose (matrix)
  "Converts a KnowRob matrix to a tf pose."
  (cl-transforms:transform->pose
   (cl-transforms:matrix->transform
    (make-array
     '(4 4) :displaced-to (make-array
                           16 :initial-contents matrix)))))

(defun has-transformation-matrix (owlid)
  (let ((owl-props (owl-has owlid)))
    (if (null owl-props)
        nil
        (let ((entries
                (loop for row below 4
                      append (loop for column below 4
                                   collect (format nil "#m~s~s" row column)))))
          (every #'(lambda (e)
                     (extract-properties e owl-props)) entries)))))

(defun owl-matrix->pose (owlid)
  (let ((owl-props (re-pl-utils:owl-has owlid)))
    (when (null (extract-properties "#m00" owl-props))
      (error "~a does not contain a transformation matrix!" owlid))
    (re-pl-utils:pl-matrix->pose
     (loop for row below 4
           append
           (loop for column below 4
                 for owl-restr = (car (re-pl-utils:extract-properties
                                       (format nil "#m~s~s" row column)
                                       owl-props))
                 collect (read-from-string (car (last (cadr owl-restr)))))))))

(defun owl-matrix->transform (owlid)
  (cl-transforms:pose->transform (owl-matrix->pose owlid)))

(defun ll-assert-1 (ll)
  (let ((ll-len (length (cut:force-ll ll))))
    (when (> ll-len 1)
      (ros-warn (ll-len) "more than 1 element found!"))
    (when (= 0 ll-len)
      (ros-warn (ll-len) "0 elements found")))
  ll)

(defun prolog->string (x)
  (string-trim "'" (symbol-name x)))

(defun string->symbol (string &key (package *package*))
  (intern (string-upcase string) package))

