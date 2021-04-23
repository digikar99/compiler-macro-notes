
(defsystem "compiler-macro-notes"
  :author "Shubhamkar B. Ayare"
  :description "Provides a macro and some conditions for use within compiler-macros"
  :license "MIT"
  :version "0.0.0" ; alpha - no versioning maintained at the moment
  :depends-on ("alexandria")
  :serial t
  :components ((:file "package")
               (:file "notes")
               (:file "with-notes")))
