
(defsystem "compiler-macro-notes"
  :author "Shubhamkar B. Ayare"
  :description "Provides a macro and some conditions for use within compiler-macros"
  :license "MIT"
  :version "0.1.0" ; beta - things are starting to look slightly stable
  :depends-on ("alexandria"
               "cl-environments")
  :serial t
  :components ((:file "package")
               (:file "notes")
               (:file "with-notes")))
