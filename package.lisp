(defpackage :compiler-macro-notes
  (:use :cl :alexandria)
  (:export
   #:*muffled-notes-type*
   #:with-notes
   #:note
   #:optimization-failure-note
   #:muffle))
