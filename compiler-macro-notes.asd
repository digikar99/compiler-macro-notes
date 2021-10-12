
(defsystem "compiler-macro-notes"
  :author "Shubhamkar B. Ayare"
  :description "Provides a macro and some conditions for use within macros and compiler-macros."
  :license "MIT"
  :version "0.2.0" ; beta - things are starting to look slightly stable
  :depends-on ("alexandria"
               "cl-environments"
               ;; cl-form-types.walker only for creating a macroexpand-all
               "cl-form-types")
  :serial t
  :components ((:file "package")
               (:file "notes")
               (:file "with-notes")))
