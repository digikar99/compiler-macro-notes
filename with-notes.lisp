(in-package :compiler-macro-notes)

(defvar *muffled-notes-type* nil
  "Bound to a type. Notes that are of type given by the value of this variable
will not be printed.
Example:
- No notes will be printed if values is T.
- Optimization notes will not be printed if values is
  COMPILER-MACRO-NOTES:OPTIMIZATION-FAILURE-NOTE

The compile time value of this variable is OR-ed with the MUFFLE declarations
to decide which notes to muffle.")

#|

Cases:
a. user RETURN-FROM something outside WITH-NOTES, eg: PF-COMPILER-MACRO
b. user RETURN-FROM WITH-NOTES

Cases:
1. user emits an unexpanded form
2. user emits an expanded form

In case a, PREVIOUS-FORM and PARENT-FORM cannot be updated. Some bindings for
these would already have been in place. These bindings would only be utilized
in case 2. In case a x 1, because the form is unexpanded, the incorrect
bindings would pose a non-issue.

And again, there is no trouble in case b.

|#

(defvar *swank-signalled-notes* nil)

(defun swank-signal (note env)
  (when (and (find-package :swank/backend)
             (not (muffled-p note))
             (not (member note *swank-signalled-notes*)))
    (signal (find-symbol "COMPILER-CONDITION" :swank/backend)
            :original-condition note
            :severity :note
            :message (format nil "~A" note)
            :source-context (if (eq 'parent-form
                                    (macroexpand-1 'parent-form env))
                                nil
                                (format nil "in:  ~S~%generated from:~%  ~S"
                                        (macroexpand-1 'parent-form env)
                                        (macroexpand-1 'previous-form env)))
            :location #+sbcl
                      (swank/sbcl::compiler-note-location note
                                                          (sb-c::find-error-context nil))
                      #-sbcl nil)
    (push note *swank-signalled-notes*)))

(defun with-notes-function (body form env
                            &key name (unwind-on-signal t)
                              (other-conditions nil)
                              (per-line-prefix "; ")
                              (optimization-note-condition t))
  (with-gensyms (s note notes muffled-notes-type name-string
                   return-form condition-signalled optimization-failure-notes)
    (once-only (form per-line-prefix)
      `(let ((,muffled-notes-type `(or ,*muffled-notes-type*
                                       ,@(declaration-information 'muffle ,env)))
             (*swank-signalled-notes* (when (find-package :swank/backend)
                                        (copy-list *swank-signalled-notes*)))
             ,notes ,condition-signalled ,optimization-failure-notes)
         (declare (ignorable ,condition-signalled))
         (unwind-protect
              (let ((,return-form
                      (block with-notes
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
                                ,@body)))))
                (if (equalp ,form ,return-form) ; no transformation occurred
                    ,return-form
                    `(cl:symbol-macrolet ((previous-form ,,form)
                                          (parent-form ,,return-form))
                       ,,return-form)))

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
           (when ,optimization-note-condition
             (dolist (,note ,optimization-failure-notes) (swank-signal ,note ,env)))
           (when (set-difference ,notes ,optimization-failure-notes)
             (dolist (,note ,notes) (swank-signal ,note ,env)))
           (let ((,s *error-output*))
             (when (and ,optimization-note-condition ,optimization-failure-notes)
               (terpri ,s)
               (pprint-logical-block (,s nil :per-line-prefix ,per-line-prefix)
                 (format ,s "(Compiler) Macro of" )
                 (let ((,name-string
                         (format nil (if (stringp ,name) "~A" " ~S ") (or ,name (first ,form)))))
                   (format ,s (if (< (length ,name-string) 38)
                                  "~A"
                                  "~&  ~A~&")
                           ,name-string))
                 (format ,s "is unable to optimize~%")
                 (pprint-logical-block (,s nil :per-line-prefix "  ")
                   (format ,s "~S" ,form))
                 (unless (eq 'parent-form
                             (macroexpand-1 'parent-form ,env))
                   (let ((parent-form (macroexpand-1 'parent-form ,env))
                         (previous-form (macroexpand-1 'previous-form ,env)))
                     (format ,s "~&in~%")
                     (pprint-logical-block (,s nil :per-line-prefix "  ")
                       (format ,s "~S" parent-form))
                     (format ,s "~&generated from~&")
                     (pprint-logical-block (,s nil :per-line-prefix "  ")
                       (format ,s "~S" previous-form))))
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
                 (when (null ,optimization-failure-notes)
                   (unless (eq 'parent-form
                               (macroexpand-1 'parent-form ,env))
                     (let ((parent-form (macroexpand-1 'parent-form ,env))
                           (previous-form (macroexpand-1 'previous-form ,env)))
                       (format ,s "~&in~%")
                       (pprint-logical-block (,s nil :per-line-prefix "  ")
                         (format ,s "~S" parent-form))
                       (format ,s "~&generated from~&")
                       (pprint-logical-block (,s nil :per-line-prefix "  ")
                         (format ,s "~S" previous-form)))))
                 (format ,s "~&  Following notes were encountered:~&")
                 (pprint-logical-block (,s nil :per-line-prefix "    ")
                   (format ,s "~{~^~%~A~}" ,notes)
                   (mapc (lambda (c) (setf (muffled-p c) t)) ,notes)))
               (terpri ,s))))))))

(defmacro with-notes ((form env
                       &rest key-args
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
- BODY is surrounded by a (BLOCK WITH-NOTES ...) on the outside
- Further, WITH-NOTES also wraps the BODY in an UNWIND-PROTECT and prints the
  conditions that were signalled before exiting. If UNWIND-ON-SIGNAL is non-NIL,
  then returns FORM if a condition was signalled, else if no condition was
  signalled returns the (primary) return value of BODY.
- If UNWIND-ON-SIGNAL is NIL, surrounds BODY in a HANDLER-BIND and prints all
  the compiler notes that were signalled. If non-NIL, prints only the first
  signalled note.
- OPTIMIZATION-FAILURE-NOTEs are printed only if OPTIMIZATION-NOTE-CONDITION
  form evaluates to non-NIL: OPTIMIZATION-NOTE-CONDITION is expected to be a
  form.
- OTHER-CONDITIONS is a type-specifier that indicates which other conditions
  should be reported.
- If the user code in BODY does result in an expansion, then it is expected to
  avoid performing a nonlocal exit to a place outside WITH-NOTES. Not
  doing so could result in an incorrect print of the expansion paths."
  (declare (ignorable name unwind-on-signal other-conditions
                      per-line-prefix optimization-note-condition))
  (apply 'with-notes-function body form env key-args))
