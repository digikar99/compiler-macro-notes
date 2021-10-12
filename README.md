# compiler-macro-notes

Another attempt at [compiler-macro](https://github.com/Bike/compiler-macro).

- Here, we do not treat compiler notes as warnings, but instead these are a separate class of conditions. These are also not errors.

- Two main condition classes are provided: [compiler-macro-notes:note](#note) and [compiler-macro-notes:optimization-failure-note](#optimization-failure-note). While the latter is a subclass of the former, the latter notes are printed in a slightly different manner to the former.

- Additional tools are provided for the printing of the expansions that led to the note to aid debugging:
  - [\*print-expansion-notes\*](#print-expansion-notes)
  - [with-expansion-notes](#with-expansion-notes)
  - [\*parent-form\*](#parent-form)
  - [macroexpand-all](#macroexpand-all)

## EXAMPLE USAGE

Notes may be muffled by
- calling [muffle](#muffle) on them; this is useful if you are not relying on [with-notes](#with-notes) to handle them
- or by using the custom declaration [compiler-macro-notes:muffle](#muffle) with the appropriate types
- or by binding [\*muffled-notes-type\*](#muffled-notes-type) to the appropriate type at compile time; this can be useful if you are relying on `eval` to - say - test something

The effect is that the notes are signalled but not printed.

```lisp

(defun foo (a b)
  (+ a b))

(define-condition not-a-number (optimization-failure-note)
  ((form :initarg :form :reader form))
  (:report (lambda (c s)
             (format s "~S is not a number" (form c)))))

(define-compiler-macro foo (&whole form a b &environment env)
  (with-notes (form env)
    (unless (numberp a)
      (signal 'not-a-number :form a))
    (unless (numberp b)
      (signal 'not-a-number :form b))
    (return-from foo (+ a b))))

(defun bar (a b) (foo a b))
; Compiler-macro of FOO is unable to optimize
;   (FOO A B)
; because:
;
;   A is not a number

(defun bar (a b)
  (declare (muffle not-a-number)) ; no note printed
  (foo a b))

(disassemble 'bar)
; disassembly for BAR
; Size: 29 bytes. Origin: #x52DB5C53                          ; BAR
; 53:       498B4510         MOV RAX, [R13+16]                ; thread.binding-stack-pointer
; 57:       488945F8         MOV [RBP-8], RAX
; 5B:       488BD6           MOV RDX, RSI
; 5E:       488BFB           MOV RDI, RBX
; 61:       B904000000       MOV ECX, 4
; 66:       FF7508           PUSH QWORD PTR [RBP+8]
; 69:       E9F42F66FD       JMP #x50418C62                   ; #<FDEFN FOO>
; 6E:       CC10             INT3 16                          ; Invalid argument count trap

(defun baz () (foo 4 5))

(disassemble 'baz)
; disassembly for BAZ
; Size: 21 bytes. Origin: #x52DB5CEC                          ; BAZ
; CEC:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; CF0:       488945F8         MOV [RBP-8], RAX
; CF4:       BA12000000       MOV EDX, 18
; CF9:       488BE5           MOV RSP, RBP
; CFC:       F8               CLC
; CFD:       5D               POP RBP
; CFE:       C3               RET
; CFF:       CC10             INT3 16                         ; Invalid argument count trap

```

## DOCUMENTATION

### \*muffled-notes-type\*

```lisp
Variable
Default Value: NIL
```

Bound to a type. Notes that are of type given by the value of this variable
will not be printed.
Example:
- No notes will be printed if values is T.
- Optimization notes will not be printed if values is
  [compiler-macro-notes:optimization-failure-note](#optimization-failure-note)

The compile time value of this variable is OR-ed with the [muffle](#muffle) declarations
to decide which notes to muffle.

### \*parent-form\*

```lisp
Variable
Default Unbound
```

While using [with-expansion-notes](#with-expansion-notes) after setting [\*print-expansion-notes\*](#print-expansion-notes) to T
at compile time, user of [with-notes](#with-notes) needs to bind *PARENT-FORM* to an
appropriate FORM, before calling [compiler-macro-notes:macroexpand-all](#macroexpand-all) on FORM
before finally emitting the expansion. An example usage is in
POLYMORPHIC-FUNCTIONS::PF-COMPILER-MACRO.

### \*print-expansion-notes\*

```lisp
Variable
Default Value: NIL
```

If non-NIL, prints notes only when the forms are surrounded by
[with-expansion-notes](#with-expansion-notes). Along with the use of [\*parent-form\*](#parent-form), this allows
[with-notes](#with-notes) to provide more helpful compiler notes, printing the expansion
forms along the way.

If NIL, prints the notes without any expansion forms that led to it.

### macroexpand-all

```lisp
Function: (macroexpand-all form &optional env)
```

### muffle

```lisp
Function: (muffle note)
```

Do not print this `note`.
As a declaration, this takes in type specifiers as arguments.

### note

```lisp
Condition
```


### optimization-failure-note

```lisp
Condition
```


### with-expansion-notes

```lisp
Macro: (with-expansion-notes &body body)
```

To print the expansions that led to the notes,
1. Set the (compile-time) value of [\*print-expansion-notes\*](#print-expansion-notes) to non-NIL
2. Surround the form(s) in `with-expansion-notes`
3. Make the code using [with-notes](#with-notes) make use of [\*parent-form\*](#parent-form) and [macroexpand-all](#macroexpand-all)
   appropriately.

To be able to print the expansions, one requires three things:
- firstly, the form before expansion, the from-form,
- secondly, the expansion itself,
- and thirdly, the sub-form in the expansion that is using [with-notes](#with-notes),
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
dynamic-binding to [\*parent-form\*](#parent-form), this information is lost as one exits the
 (COMPILER) MACRO. Thus, one way to allow [with-notes](#with-notes) to make use of this
information is to bind the [\*parent-form\*](#parent-form) to the expansion and call
[macroexpand-all](#macroexpand-all) on the not fully-expanded expansion. Because the to-form of the
current stage is the from-form of the next stage of expansion, this is taken
care of in recursive calls to [with-notes](#with-notes) during [macroexpand-all](#macroexpand-all).

Another analogy about from-form and to-form is a path with labelled edges; with
nodes as the from-form and to-form and the edge-labels as the expansion of the
from-form.

### with-notes

```lisp
Macro: (with-notes
        (form env &key (name) (unwind-on-signal t) (other-conditions NIL)
         (per-line-prefix ; ) (optimization-note-condition t))
        &body body)
```

A macro to readably signal [compiler-macro-notes:note](#note) for end-users:
- Expects `env` to evaluate to an environment suitable for passing to
  CL-ENVIRONMENTS.CLTL2:DEFINE-DECLARATION
- Wraps the `body` in an UNWIND-PROTECT and prints the conditions that were signalled
  before exiting. If `unwind-on-signal` is non-NIL, then returns `form` if a condition was
  signalled, else if no condition was signalled returns the (primary) return value of `body`.
- If `unwind-on-signal` is NIL, surrounds `body` in a HANDLER-BIND and prints all the compiler
  notes that were signalled. If non-NIL, prints only the first signalled note.
- [optimization-failure-note](#optimization-failure-note)s are printed only if `optimization-note-condition` form evaluates
  to non-NIL: `optimization-note-condition` is expected to be a form.
- `other-conditions` is a type-specifier that indicates which other conditions should
  be reported.
