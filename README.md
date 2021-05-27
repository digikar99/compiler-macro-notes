# compiler-macro-notes

Another attempt at [compiler-macro](https://github.com/Bike/compiler-macro).

- Here, we do not treat compiler notes as warnings, but instead these are a separate class of conditions. These are also not errors.

- Two main condition classes are provided: `compiler-macro-notes:note` and `compiler-macro-notes:optimization-failure-note`. While the latter is a subclass of the former, the latter notes are printed in a slightly different manner to the former.

### Example Usage

Notes may be muffled by calling `muffle` on them, or by binding `compiler-macro-notes:*muffled-notes*` to the appropriate type. The effect is that the notes are signalled but not printed.

```lisp

(defun foo (a b)
  (+ a b))

(define-condition not-a-number (optimization-failure-note)
  ((form :initarg :form :reader form))
  (:report (lambda (c s)
             (format s "~S is not a number" (form c)))))

(define-compiler-macro foo (&whole form a b)
  (with-notes (form)
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
