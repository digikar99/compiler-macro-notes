(in-package :compiler-macro-notes)

(define-condition note (condition)
  ((datum :initarg :datum :reader datum)
   (args  :initarg :args  :reader args)
   (muffled-p :accessor muffled-p :initform nil))
  (:report (lambda (condition stream)
             (apply #'format stream (datum condition) (args condition)))))

(define-condition optimization-failure-note (note) ())

(defun muffle (note) (setf (muffled-p note) t))

(define-declaration muffle-notes (names env)
  (values :declare
          (cons 'muffle-notes
                (append names (declaration-information 'muffle-notes env)))))
