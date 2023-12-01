(defpackage #:compiler-macro-notes
  (:use :cl :alexandria :cl-environments.cltl2)
  (:export
   #:note
   #:optimization-failure-note
   #:muffle
   #:*muffled-notes-type*
   #:with-notes
   #:root-form
   #:parent-form
   #:augment-expansion-path))
