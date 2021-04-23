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
                         (per-line-prefix "; ")
                         (optimization-note-condition t))
                      &body body)
  "A macro to readably signal COMPILER-MACRO-NOTES:NOTE for end-users:
- Returns FORM. Use a RETURN-FROM as usual within functions to return another return value.
- If UNWIND-ON-SIGNAL is NIL, surrounds BODY in a HANDLER-BIND and prints all the compiler
  notes that were signalled. If non-NIL, prints only the first signalled note.
- OPTIMIZATION-FAILURE-NOTEs are printed only if OPTIMIZATION-NOTE-CONDITION form evaluates
  to non-NIL: OPTIMIZATION-NOTE-CONDITION is expected to be a form."
  (with-gensyms (s note notes optimization-failure-notes)
    (once-only (form per-line-prefix)
      `(let (,notes ,optimization-failure-notes)
         ,(if unwind-on-signal
              `(handler-case (progn ,@body)
                 (optimization-failure-note (,note)
                   (push ,note ,optimization-failure-notes)
                   (push ,note ,notes))
                 (note (,note)
                   (push ,note ,notes)))
              `(handler-bind ((note
                                (lambda (,note)
                                  (push ,note ,notes)))
                              (optimization-failure-note
                                (lambda (,note)
                                  (push ,note ,optimization-failure-notes))))
                 ,@body))
         (setq ,notes
               (remove-if (lambda (c) (typep c *muffled-notes*))
                          ,notes))
         (setq ,optimization-failure-notes
               (remove-if (lambda (c) (typep c *muffled-notes*))
                          ,optimization-failure-notes))
         (nreversef ,notes)
         (nreversef ,optimization-failure-notes)
         (let ((,s *error-output*))
           (when (and ,optimization-note-condition ,optimization-failure-notes)
             (terpri ,s)
             (pprint-logical-block (,s nil :per-line-prefix ,per-line-prefix)
               (format ,s "Compiler-macro of ~S is unable to optimize~%" (or ',name
                                                                             (first ,form)))
               (pprint-logical-block (,s nil :per-line-prefix "  ")
                 (format ,s "~S" ,form))
               (format ,s "~&because:~&")
               (pprint-logical-block (,s nil :per-line-prefix "  ")
                 (format ,s "~{~^~%~A~}" ,optimization-failure-notes))))
           (when (set-difference ,notes ,optimization-failure-notes)
             (terpri ,s)
             (pprint-logical-block (,s nil :per-line-prefix ,per-line-prefix)
               (format ,s "While compiling~%")
               (pprint-logical-block (,s nil :per-line-prefix "    ")
                 (format ,s "~S" ,form))
               (format ,s "~&  Following COMPILER-MACRO-NOTES:NOTE were encountered:~&")
               (pprint-logical-block (,s nil :per-line-prefix "    ")
                 (format ,s "~{~^~%~A~}" ,notes)))))
         ,form))))
