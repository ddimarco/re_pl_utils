(defsystem re-pl-utils
  :depends-on (roslisp cram-language cram-json-prolog cl-transforms)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "re-pl-utils" :depends-on ("package"))
             (:file "owl-utils" :depends-on ("re-pl-utils"))
             (:file "knowrob-utils" :depends-on ("owl-utils"))
             (:file "srdl-utils" :depends-on ("knowrob-utils"))))))
