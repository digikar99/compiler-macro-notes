(defpackage :compiler-macro-notes
  (:use :cl :alexandria :cl-environments.cltl2)
  (:export
   #:muffle-notes
   #:*muffled-notes-type*
   #:with-notes
   #:note
   #:optimization-failure-note
   #:muffle))
