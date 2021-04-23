(in-package :compiler-macro-notes)

(define-condition note (condition)
  ((datum :initarg :datum :reader datum)
   (args  :initarg :args  :reader args))
  (:report (lambda (condition stream)
             (apply #'format stream (datum condition) (args condition)))))

(define-condition optimization-failure-note (note) ())
