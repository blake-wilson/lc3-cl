(defparameter *memory* (make-array (expt 2 16)))

;(defparameter *registers*
;  '(:R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7 :R_COND :R_PC))

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


(defparameter *reg* (make-array 10))

(defparameter *op-codes*
  '(:OP_BR :OP_ADD :OP_LD :OP_ST :OP_JSR :OP_AND :OP_LDR
           :OP_STR :OP_RTI :OP_NOT :OP_LDI :OP_STI :OP_JMP :OP_RES :OP_LEA
           :OP_TRAP))


(defconstant FL_POS (ash 1 0)) ; P
(defconstant FL_ZRO (ash 1 1)) ; Z
(defconstant FL_NEG (ash 1 2)) ; N

