(defpackage :compiler-macro-notes
  (:use :cl :alexandria :cl-environments.cltl2)
  (:export
   #:*muffled-notes-type*
   #:with-notes
   #:note
   #:optimization-failure-note
   #:muffle

   #:*print-expansion-notes*
   #:with-expansion-notes
   #:*parent-form*
   #:macroexpand-all))
