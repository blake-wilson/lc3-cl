(declaim (optimize (debug 3)))
(load "memory.lisp")

(defparameter *running* t)

(defun make-memory ()
  (make-array *memory-max* :element-type '(unsigned-byte 16)))

(defun (setf memory) (value mem address)
  (setf (aref mem address) value))

(defun memory (memory address)
  (aref memory address))

(defun update-flags (registers to-update)
  (cond
    ((eq to-update 0)
      (setf (aref registers R_COND) FL_ZRO))
    ((eq (logand 1 (ash to-update -15)) 1)
      (setf (aref registers R_COND) FL_NEG))
    (t
      (setf (aref registers R_COND) FL_POS))
  ))

(defun register (registers register)
  (aref registers register))

(defun (setf register) (value registers register)
  (setf (aref registers register) value)
  (if (not (eq register R_PC))
        (update-flags registers value)
  ))

(defun make-registers ()
  (make-array 10 :element-type '(unsigned-byte 16)))

(defun with-overflow (val)
  (ldb (byte 16 0) val))

(defun fetch-instruction (instr)
    (nth (ash instr -12) *op-codes*))

(defun sign-extend (x bits)
  (if (= (ldb (byte 1 (1- bits)) x) 0)
      x
      (1+ (logxor (ash #xFFFF (+ -16 bits)) (- x)))))

(defmacro with-spec (instr spec &body body)
  `(let ,(loop for (name start-bit stop-bit) in spec
              collect (list name `(logand (ash ,instr (- ,start-bit))
                              (1- (ash 1 (1+ (- ,stop-bit ,start-bit)))))
                      )
        )
    ,@body
   ))

(defun expand-spec (instr spec)
  (values (loop for (name start-bit stop-bit) in spec
        collect (list name (logand (ash instr (- start-bit))
                           (1- (ash 1 (- stop-bit start-bit)))))
  )))

(defun add (instr registers) (with-spec instr ((dr 9 11) (sr1 6 8) (imm-flag 5 5) (imm 0 4) (sr2 0 2))
  (if (not (eq imm-flag 0))
      (setf (register registers dr) (with-overflow (+ (aref registers sr1) (sign-extend imm 5))))
      (setf (register registers dr) (with-overflow (+ (aref registers sr1) (aref registers sr2))))
  )))

(defun load-indirect (instr registers memory) (with-spec instr ((dr 9 11) (pc-offset 0 8))
    (setf (register registers dr)
          (mem-read memory (mem-read memory (+ (register registers R_PC) (sign-extend pc-offset 9))))
    )))

(defun bitwise-and (instr registers) (with-spec instr ((dr 9 11) (sr1 6 8) (imm-flag 5 5) (imm 0 4) (sr2 0 2))
  (if (eq imm-flag 1)
    (setf (register registers dr) (logand (register registers sr1) (sign-extend imm 5)))
    (setf (register registers dr) (logand (register registers sr1) (register registers sr2)))
  )))

(defun not-instr (instr registers) (with-spec instr ((dr 9 11) (sr 6 8))
  (setf (register registers dr) (logxor #xFFFF (register registers sr)))))

(defun branch (instr registers) (with-spec instr ((cond-flag 9 11) (pc-offset 0 8))
  (if (not (eq (logand cond-flag (register registers R_COND)) 0))
        (incf (aref registers R_PC) (sign-extend pc-offset 9))
  )))

(defun jump (instr registers) (with-spec instr ((br 6 8))
    (setf (register registers R_PC) (register registers br))))

(defun jump-register (instr registers) (with-spec instr ((long-flag 11 11) (pc-offset 0 10) (br 6 8))
  (setf (register registers R7) (register registers R_PC))
  (if (eq long-flag 1)
    (incf (aref registers R_PC) (sign-extend pc-offset 11))
    (setf (register registers R_PC) (aref registers br))
  )))

(defun load-instr (instr registers memory) (with-spec instr ((dr 9 11) (pc-offset 0 8))
  (setf (register registers dr) (mem-read memory
                                (+ (aref registers R_PC) (sign-extend pc-offset 9))))
  ))

(defun load-register (instr registers memory) (with-spec instr ((dr 9 11) (br 6 8) (offset 0 5))
  (setf (register registers dr) (mem-read memory (+ (aref registers br) offset)))))

(defun load-effective-address (instr registers) (with-spec instr ((dr 9 11) (pc-offset 0 8))
  (setf (register registers dr) (+ (aref registers R_PC) (sign-extend pc-offset 9)))))

(defun store (instr registers memory) (with-spec instr ((sr 9 11) (pc-offset 0 8))
  (mem-write memory
             (+ (register registers R_PC)
                (sign-extend pc-offset 9)) (register registers sr))))

(defun store-indirect (instr registers memory) (with-spec instr ((sr 9 11) (pc-offset 0 8))
  (mem-write memory (mem-read memory (+ (register registers R_PC) (sign-extend pc-offset 9))) (register registers sr))))


(defun store-register (instr registers memory) (with-spec instr ((sr 9 11) (br 6 8) (offset 0 5))
  (mem-write memory (+ (register registers br) (sign-extend offset 6)) (register registers sr))))

(defun trap-get-c (registers)
  (let ((c (get-c)))
    (setf (register registers R0) (the (unsigned-byte 16) c))
  )
)

(defun trap-out (reg)
  (put-c (the (signed-byte 8) (aref reg R0)))
)

(defun trap-in (registers)
  (progn
    (format t "Enter a character: ")
    (let ((c (get-c)))
      (put-c (the (signed-byte 8) c))
      (setf (register registers R0) (the (unsigned-byte 16) c))
    )
  ))

(defun trap-puts (reg memory)
  (let ((c (register reg R0)))
    (loop while (not (eq (aref memory c) #x0000)) do
      (progn
        (put-c (the (signed-byte 8) (logand (aref memory c) #xff)))
        (incf c)
      )
    )
  ))

(defun trap-putsp (memory)
  (let ((c (aref memory R0)))
    (loop while (not (eq (aref memory c) #x00)) do
      (let ((char1 (logand c #xff))
            (char2 (ash c -8)))
            (put-c (the (signed-byte 8) char1))
            (if (not (eq 0 char2))
              (put-c (the (signed-byte 8) char2))
            )
      )
      (incf c)
    )
  ))

(defun trap-halt ()
  (restore-input-buffering)
  (setf *running* nil))

(defun trap (instr registers memory) (with-spec instr ((code 0 7))
  (cond ((eq code TRAP_GETC) (trap-get-c registers))
        ((eq code TRAP_OUT) (trap-out registers))
        ((eq code TRAP_PUTS) (trap-puts registers memory))
        ((eq code TRAP_IN) (trap-in registers))
        ((eq code TRAP_PUTSP) (trap-putsp memory))
        ((eq code TRAP_HALT) (trap-halt))
    )))

(defun swap16 (value)
  (logior (logand (ash value 8) #xFF00) (ash value -8)))

(defun read-origin (file)
  (let ((origin-bytes (make-array 1 :element-type '(unsigned-byte 16))))
    (read-sequence origin-bytes file)
    (swap16 (aref origin-bytes 0))
  )) 

(defun read-image-file (file memory)
  (let* ((origin (read-origin file))
         (max-read (- *memory-max* origin))
         (ptr origin)
         (read (read-sequence memory file :start origin :end max-read))
        )
    (loop while (> (decf read) 0) do
        (progn
          (mem-write memory ptr (swap16 (aref memory ptr)))
          (incf ptr)
        )
    )
    origin
  ))

(defun read-image (image-path memory)
  "Reads file at the provided path into the provided memory"
    (let ((image-stream))
      (setf image-stream (open image-path :direction :input :element-type '(unsigned-byte 16)))
      (let ((origin (read-image-file image-stream memory)))
        (close image-stream)
        origin)))

(defun run-vm (memory registers origin)
  (disable-input-buffering)
  ; since exactly one condition flag should be set at any given time, set the Z flag
  (setf (aref registers R_COND) FL_ZRO)

  ; set the PC to starting position
  (setf (aref registers R_PC) origin)

  (loop while *running*
    do (let* (
             (instr (mem-read memory (aref registers R_PC)))
             (op (fetch-instruction instr))
          )
         (incf (register registers R_PC) 1)
         (case op 
               (:OP_BR (branch instr registers))
               (:OP_ADD (add instr registers))
               (:OP_LD (load-instr instr registers memory))
               (:OP_ST (store instr registers memory))
               (:OP_JSR (jump-register instr registers))
               (:OP_AND (bitwise-and instr registers))
               (:OP_LDR (load-register instr registers memory))
               (:OP_STR (store-register instr registers memory))
               (:OP_RTI ())
               (:OP_NOT (not-instr instr registers))
               (:OP_LDI (load-indirect instr registers memory))
               (:OP_STI (store-indirect instr registers memory))
               (:OP_JMP (jump instr registers))
               (:OP_RES ())
               (:OP_LEA (load-effective-address instr registers))
               (:OP_TRAP (trap instr registers memory))
               (t (format t "unrecognized instruction ~A" instr))
          )
       )
   )
)

(defun main ()
  (let ((args *posix-argv*)
        (memory (make-memory))
        (registers (make-registers))
       )
    (if (< (length args) 2)
      (progn
        (format t "No file provided. Provide the filename of an executable object file~%")
        (exit)
      )
    )
    (let ((origin (read-image (nth 1 args) memory)))
      (run-vm memory registers origin)
    )
  ))

(defun run-image (image-path)
  (let* ((memory (make-memory))
        (registers (make-registers))
        (origin (read-image image-path memory)))
    (run-vm memory registers origin)
  ))

