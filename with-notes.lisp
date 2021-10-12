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

(defvar *forms* nil)

(defvar *print-expansion-notes* nil
  "If non-NIL, prints notes only when the forms are surrounded by
WITH-EXPANSION-NOTES. Along with the use of *PARENT-FORM*, this allows
WITH-NOTES to provide more helpful compiler notes, printing the expansion
forms along the way.

If NIL, prints the notes without any expansion forms that led to it.")

(defvar *parent-form*)
(setf (documentation '*parent-form* 'variable)
      "While using WITH-EXPANSION-NOTES after setting *PRINT-EXPANSION-NOTES* to T
at compile time, user of WITH-NOTES needs to bind *PARENT-FORM* to an
appropriate FORM, before calling COMPILER-MACRO-NOTES:MACROEXPAND-ALL on FORM
before finally emitting the expansion. An example usage is in
POLYMORPHIC-FUNCTIONS::PF-COMPILER-MACRO.")

(defvar *expanding-within-with-expansion-notes* nil)

(defun macroexpand-all (form &optional env)
  (cl-form-types.walker:walk-form (lambda (form env)
                                    (if (and *print-expansion-notes*
                                             (listp form)
                                             (typep (first form) 'symbol)
                                             (compiler-macro-function (first form)))
                                        (progn
                                          (values (nth-value 0 (funcall
                                                                (compiler-macro-function (first form))
                                                                form
                                                                env))
                                                  t))
                                        form))
                                  form
                                  env))

(defmacro with-expansion-notes (&body body)
  ;; No ENV because expected to be a top-level form
  "To print the expansions that led to the notes,
1. Set the (compile-time) value of *PRINT-EXPANSION-NOTES* to non-NIL
2. Surround the form(s) in WITH-EXPANSION-NOTES
3. Make the code using WITH-NOTES make use of *PARENT-FORM* and MACROEXPAND-ALL
   appropriately.

To be able to print the expansions, one requires three things:
- firstly, the form before expansion, the from-form,
- secondly, the expansion itself,
- and thirdly, the sub-form in the expansion that is using WITH-NOTES,
  call this the to-form for the below discussion.

In the absence of the use of this macro, a MACRO or COMPILER-MACRO only has
access to the to-form. It does not have access to the expansion aka the
parent-form of the to-form, and neither to the from-form which was the form
that got expanded to the to-form.

However, it is also the case that the to-form that a MACRO or COMPILER-MACRO
receives through its &WHOLE argument expands to the expansion, and within this
expansion there exist the to-form of the next stage of the (compiler)
macroexpansion process. Thus, the (COMPILER) MACRO effectively has access to the
from-form of the *next* stage of the expansion. If its expansion is stored as a
dynamic-binding to *PARENT-FORM*, this information is lost as one exits the
 (COMPILER) MACRO. Thus, one way to allow WITH-NOTES to make use of this
information is to bind the *PARENT-FORM* to the expansion and call
MACROEXPAND-ALL on the not fully-expanded expansion. Because the to-form of the
current stage is the from-form of the next stage of expansion, this is taken
care of in recursive calls to WITH-NOTES during MACROEXPAND-ALL.

Another analogy about from-form and to-form is a path with labelled edges; with
nodes as the from-form and to-form and the edge-labels as the expansion of the
from-form."
  `(progn
     ,@(loop :for form :in body
             :collect (let ((*expanding-within-with-expansion-notes* t)
                            (*forms* (list form)))
                        (macroexpand-all form)))))

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
  (with-gensyms (s note notes muffled-notes-type name-string
                   return-form condition-signalled optimization-failure-notes)
    (once-only (form per-line-prefix)
      `(let ((,muffled-notes-type `(or ,*muffled-notes-type*
                                       ,@(declaration-information 'muffle ,env)))
             ,notes ,condition-signalled ,optimization-failure-notes
             (*forms* (cons ,form *forms*)))
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
           (when (or (not *print-expansion-notes*)
                     *expanding-within-with-expansion-notes*)
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
                   (when (and *print-expansion-notes*
                              (boundp '*parent-form*))
                     (format ,s "~&in~%")
                     (pprint-logical-block (,s nil :per-line-prefix "  ")
                       (format ,s "~S" *parent-form*))
                     (format ,s "~&generated from~&")
                     (pprint-logical-block (,s nil :per-line-prefix "  ")
                       (format ,s "~S" (second *forms*))))
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
                   (when (and *print-expansion-notes*
                              (null ,optimization-failure-notes)
                              (boundp '*parent-form*))
                     (format ,s "~&in~%")
                     (pprint-logical-block (,s nil :per-line-prefix "  ")
                       (format ,s "~S" *parent-form*))
                     (format ,s "~&generated from~&")
                     (pprint-logical-block (,s nil :per-line-prefix "  ")
                       (format ,s "~S" (second *forms*))))
                   (format ,s "~&  Following notes were encountered:~&")
                   (pprint-logical-block (,s nil :per-line-prefix "    ")
                     (format ,s "~{~^~%~A~}" ,notes)
                     (mapc (lambda (c) (setf (muffled-p c) t)) ,notes)))
                 (terpri ,s)))))))))
