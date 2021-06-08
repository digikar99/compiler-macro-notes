(defpackage :compiler-macro-notes
  (:use :cl :alexandria :cl-environments.cltl2)
  (:export
   #:muffle-notes
   #:with-notes
   #:note
   #:optimization-failure-note
   #:muffle))
