(in-package :compiler-macro-notes)

(defmacro with-notes ((form env
                       &key
                         (name)
                         (unwind-on-signal t)
                         (other-conditions nil)
                         (per-line-prefix "; ")
                         (optimization-note-condition t))
                      &body body)
  "A macro to readably signal COMPILER-MACRO-NOTES:NOTE for end-users:
- Expects ENV to evaluate to an environment suitable for passing to
  CL-ENVIRONMENTS.CLTL2:DEFINE-DECLARATION
- Wraps the BODY in an UNWIND-PROTECT and prints the conditions that were signalled
  before exiting. If UNWIND-ON-SIGNAL is non-NIL, then returns FORM if a condition was
  signalled, else if no condition was signalled returns the (primary) return value of BODY.
- If UNWIND-ON-SIGNAL is NIL, surrounds BODY in a HANDLER-BIND and prints all the compiler
  notes that were signalled. If non-NIL, prints only the first signalled note.
- OPTIMIZATION-FAILURE-NOTEs are printed only if OPTIMIZATION-NOTE-CONDITION form evaluates
  to non-NIL: OPTIMIZATION-NOTE-CONDITION is expected to be a form.
- OTHER-CONDITIONS is a type-specifier that indicates which other conditions should
  be reported."
  (with-gensyms (s note notes muffled-notes-type
                   return-form condition-signalled optimization-failure-notes)
    (once-only (form per-line-prefix)
      `(let ((,muffled-notes-type `(or ,@(declaration-information 'muffle-notes ,env)))
             ,notes ,condition-signalled ,optimization-failure-notes)
         (declare (ignorable ,condition-signalled))
         (unwind-protect
              ,(if unwind-on-signal
                   `(progn
                      (let ((,return-form (handler-case (progn ,@body)
                                           (optimization-failure-note (,note)
                                             (push ,note ,optimization-failure-notes)
                                             (push ,note ,notes)
                                             (setq ,condition-signalled t))
                                           (note (,note)
                                             (push ,note ,notes)
                                             (setq ,condition-signalled t))
                                           (,other-conditions (,note)
                                             (push ,note ,notes)
                                             (setq ,condition-signalled t)))))
                        (if ,condition-signalled
                            ,form
                            ,return-form)))
                   `(handler-bind ((condition
                                     (lambda (,note)
                                       (when (or (typep ,note 'note)
                                                 (typep ,note ',other-conditions))
                                         (push ,note ,notes))))
                                   (optimization-failure-note
                                     (lambda (,note)
                                       (push ,note ,optimization-failure-notes))))
                      ,@body))
           (setq ,notes (remove-duplicates ,notes))
           (setq ,notes
                 (remove-if (lambda (c) (or (typep c ,muffled-notes-type)
                                            (and (typep c 'note)
                                                 (muffled-p c))))
                            ,notes))
           (setq ,optimization-failure-notes (remove-duplicates ,optimization-failure-notes))
           (setq ,optimization-failure-notes
                 (remove-if (lambda (c) (or (typep c ,muffled-notes-type)
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
                   (mapc (lambda (c) (setf (muffled-p c) t)) ,optimization-failure-notes)))
               (terpri ,s))
             (when (set-difference ,notes ,optimization-failure-notes)
               (unless ,optimization-failure-notes (terpri ,s))
               (pprint-logical-block (,s nil :per-line-prefix ,per-line-prefix)
                 (format ,s "While compiling~%")
                 (pprint-logical-block (,s nil :per-line-prefix "    ")
                   (format ,s "~S" ,form))
                 (format ,s "~&  Following notes were encountered:~&")
                 (pprint-logical-block (,s nil :per-line-prefix "    ")
                   (format ,s "~{~^~%~A~}" ,notes)
                   (mapc (lambda (c) (setf (muffled-p c) t)) ,notes)))
               (terpri ,s))))))))
