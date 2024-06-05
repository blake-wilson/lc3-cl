(declaim (optimize (debug 3)))

(defparameter *memory-max* (ash 1 16))

(defconstant R0 0)
(defconstant R1 1)
(defconstant R2 2)
(defconstant R3 3)
(defconstant R4 4)
(defconstant R5 5)
(defconstant R6 6)
(defconstant R7 7)
(defconstant R_COND 8)
(defconstant R_PC 9)

(defconstant TRAP_GETC #x20)
(defconstant TRAP_OUT #x21)
(defconstant TRAP_PUTS #x22)
(defconstant TRAP_IN #x23)
(defconstant TRAP_PUTSP #x24)
(defconstant TRAP_HALT #x25)

(defconstant MR_KBSR #xFE00) ; keyboard status
(defconstant MR_KBDR #xFE02) ; keyboard data

(defparameter *reg* (make-array 10 :element-type '(unsigned-byte 16)))

(defparameter *op-codes*
  '(:OP_BR :OP_ADD :OP_LD :OP_ST :OP_JSR :OP_AND :OP_LDR
           :OP_STR :OP_RTI :OP_NOT :OP_LDI :OP_STI :OP_JMP :OP_RES :OP_LEA
           :OP_TRAP))

(defconstant FL_POS (ash 1 0)) ; P
(defconstant FL_ZRO (ash 1 1)) ; Z
(defconstant FL_NEG (ash 1 2)) ; N

(require 'asdf)
(asdf:load-system :cffi)

(use-package 'cffi)

(pushnew (truename ".") *foreign-library-directories*
         :test #'equal)
 
(load-foreign-library '(:default "libkeyboard"))
(defcfun ("check_key" check-key-c) :int)
(defcfun ("disable_input_buffering") :void)
(defcfun ("restore_input_buffering") :void)

; c functions from stdio. Used instead of Common Lisp's
; char writing functions
(defcfun ("put_c") :void (char :char))
(defcfun ("get_c") :char)

(defun check-key ()
  (not (eq 0 (check-key-c)))
)

(defun mem-write (memory address val)
  (setf (aref memory address) val))

(defun mem-read (memory address)
  (if (eq address MR_KBSR)
    (if (check-key)
      (progn
        (setf (aref memory MR_KBSR) (ash 1 15))
        (setf (aref memory MR_KBDR) (the (unsigned-byte 16) (get-c)))
      )
      (setf (aref memory MR_KBSR) #x00)
    )
  )
  (aref memory address))
