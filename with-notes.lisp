(in-package :compiler-macro-notes)

(defvar *muffled-notes* nil
  "Bound to a type. Notes that are of type given by the value of this variable
will not be printed.
Example:
- No notes will be printed if values is T.
- Optimization notes will not be printed if values is
  COMPILER-MACRO-NOTES:OPTIMIZATION-FAILURE-NOTE")

(defmacro with-notes ((form
                       &key
                         (name)
                         (unwind-on-signal t)
                         (other-conditions nil)
                         (per-line-prefix "; ")
                         (optimization-note-condition t))
                      &body body)
  "A macro to readably signal COMPILER-MACRO-NOTES:NOTE for end-users:
- Wraps the BODY in an UNWIND-PROTECT and prints the conditions that were signalled
  before exiting. If UNWIND-ON-SIGNAL is non-NIL, returns FORM.
- If UNWIND-ON-SIGNAL is NIL, surrounds BODY in a HANDLER-BIND and prints all the compiler
  notes that were signalled. If non-NIL, prints only the first signalled note.
- OPTIMIZATION-FAILURE-NOTEs are printed only if OPTIMIZATION-NOTE-CONDITION form evaluates
  to non-NIL: OPTIMIZATION-NOTE-CONDITION is expected to be a form.
- OTHER-CONDITIONS is a type-specifier that indicates which other conditions should
  be reported."
  (with-gensyms (s note notes optimization-failure-notes)
    (once-only (form per-line-prefix)
      `(let (,notes ,optimization-failure-notes)
         (unwind-protect
              ,(if unwind-on-signal
                   `(progn
                      (handler-case (progn ,@body)
                        (optimization-failure-note (,note)
                          (push ,note ,optimization-failure-notes)
                          (push ,note ,notes))
                        (note (,note)
                          (push ,note ,notes))
                        (condition (,note)
                          (when (typep ,note ',other-conditions)
                            (push ,note ,notes))))
                      ,form)
                   `(handler-bind ((condition
                                     (lambda (,note)
                                       (when (or (typep ,note 'note)
                                                 (typep ,note ',other-conditions))
                                         (push ,note ,notes))))
                                   (optimization-failure-note
                                     (lambda (,note)
                                       (push ,note ,optimization-failure-notes))))
                      ,@body))
           (setq ,notes
                 (remove-if (lambda (c) (or (typep c *muffled-notes*)
                                            (and (typep c 'note)
                                                 (muffled-p c))))
                            ,notes))
           (setq ,optimization-failure-notes
                 (remove-if (lambda (c) (or (typep c *muffled-notes*)
                                            (and (typep c 'note)
                                                 (muffled-p c))))
                            ,optimization-failure-notes))
           (nreversef ,notes)
           (nreversef ,optimization-failure-notes)
           (let ((,s *error-output*))
             (when (and ,optimization-note-condition ,optimization-failure-notes)
               (terpri ,s)
               (pprint-logical-block (,s nil :per-line-prefix ,per-line-prefix)
                 (format ,s "Compiler-macro of ~S is unable to optimize~%" (or ,name
                                                                               (first ,form)))
                 (pprint-logical-block (,s nil :per-line-prefix "  ")
                   (format ,s "~S" ,form))
                 (format ,s "~&because:~&")
                 (pprint-logical-block (,s nil :per-line-prefix "  ")
                   (format ,s "~{~^~%~A~}" ,optimization-failure-notes)
                   (mapc (lambda (c) (setf (muffled-p c) t)) ,optimization-failure-notes))))
             (when (set-difference ,notes ,optimization-failure-notes)
               (terpri ,s)
               (pprint-logical-block (,s nil :per-line-prefix ,per-line-prefix)
                 (format ,s "While compiling~%")
                 (pprint-logical-block (,s nil :per-line-prefix "    ")
                   (format ,s "~S" ,form))
                 (format ,s "~&  Following notes were encountered:~&")
                 (pprint-logical-block (,s nil :per-line-prefix "    ")
                   (format ,s "~{~^~%~A~}" ,notes)
                   (mapc (lambda (c) (setf (muffled-p c) t)) ,notes)))
               (terpri ,s))))))))
