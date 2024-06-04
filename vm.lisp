(declaim (optimize (debug 3)))
(load "memory.lisp")

(defparameter *running* t)

(defun make-memory ()
  (make-array *memory-max* :element-type '(unsigned-byte 16)))

(defun make-registers ()
  (make-array 10 :element-type '(unsigned-byte 16)))

(defun main ()
  (let ((args *posix-argv*)
        (memory (make-memory))
        (registers (make-registers))
       )
    (loop for arg in args do
          (format t "~A" arg)
    )
    (if (< (length args) 2)
      (progn
        (format t "No file provided. Provide the filename of an executable object file~%")
        (exit)
      )
    )
    (read-image (nth 1 args) memory)
    (run-vm memory registers)
  ))

(defun run-vm (memory registers)
  (disable-input-buffering)
  ; since exactly one condition flag should be set at any given time, set the Z flag
  (setf (aref reg R_COND) FL_ZRO)

  ; set the PC to starting position
  ; 0x3000 is the default
  (setf (aref reg R_PC) #x3000)

  (loop while *running*
    do (let* (
             (instr (mem-read (aref reg R_PC)))
             (op (fetch-instruction instr))
          )
         (incf (aref reg R_PC) 1)
         (cond 
               ((eq op OP_BR) (branch instr))
               ((eq op OP_ADD) (add instr))
               ((eq op OP_LD) (load-instr instr))
               ((eq op OP_ST) (store instr))
               ((eq op OP_JSR) (jump-register instr))
               ((eq op OP_AND) (bitwise-and instr))
               ((eq op OP_LDR) (load-register instr))
               ((eq op OP_STR) (store-register instr))
               ((eq op OP_RTI) ())
               ((eq op OP_NOT) (not-instr instr))
               ((eq op OP_LDI) (load-indirect instr))
               ((eq op OP_STI) (store-indirect instr))
               ((eq op OP_JMP) (jump instr))
               ((eq op OP_RES) ())
               ((eq op OP_LEA) (load-effective-address instr))
               ((eq op OP_TRAP) (trap instr))
               (t (format t "unrecognized instruction ~A" instr))
          )
       )
   )
)

(defun with-overflow (val)
  (ldb (16 0) val))

(defun fetch-instruction (instr)
    (ash instr -12))

(defun sign-extend (x bits)
  (if (= (ldb (byte 1 (1- bits)) x) 0)
      x
      (1+ (logxor (ash #xFFFF (+ -16 bits)) (- x)))))

(defun update-flags (registers to-update)
  (cond
    ((eq to-update 0)
      (setf (aref registers R_COND) FL_ZRO))
    ((eq (logand 1 (ash to-update -15)) 1)
      (setf (aref registers R_COND) FL_NEG))
    (t
      (setf (aref registers R_COND) FL_POS))
  ))


(defun set-reg (registers reg val)
  (setf reg val)
  (update-flags registers reg))

; 
(defconstant add-spec '((dr 9 11) (sr1 6 8) (imm-flag 5 5) (imm 0 5) ))

(defmacro with-spec (args &body body)
  (let ((values (loop for (name start-bit stop-bit) in args
      collect (logand (ash instr -start-bit)
                           (1- (ash (- stop-bit start-bit))))
    )))
  )


(defun add (dr sr1 &optional sr2 &optional imm)
  (if (not sr2)
    (let ((imm5 (sign-extend (logand instr #x1F) 5)))
      (set-reg registers dr (with-overflow (+ (aref reg sr1) imm)))
    )
    (let ((r-r2 (logand instr #x07)))
      (set-reg registers dr (with-overflow (+ (aref reg sr1) (aref reg sr2))))
    )
  ))

(defun load-indirect (instr reg)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (pc-offset (sign-extend (logand instr #x1FF) 9)))
       (progn
         (setf (aref reg r-r0)
               (mem-read (mem-read (+ (aref reg R_PC) pc-offset))))
         (update-flags r-r0)
       )
    )
  )

(defun bitwise-and (instr reg)
  (let ((r-r0 (logand (ash instr -9) #x07))
      (r-r1 (logand (ash instr -6) #x07))
      (imm-flag (logand (ash instr 5) #x01)))
      (if (eq imm-flag 1)
        (let ((imm5 (sign-extend (logand instr #x1F) 5)))
          (setf (aref reg r-r0) (logand (aref reg r-r1) imm5))
        )
        (let ((r-r2 (logand instr #x07)))
          (setf (aref reg r-r0) (logand (aref reg r-r1) r-r2))
        )
      )
      (update-flags r-r0)
    ))

(defun not-instr (instr reg)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (r-r1 (logand (ash instr -6) #x07)))

        (setf (aref reg r-r0) (logxor #xFFFF (aref reg r-r1)))
        (update-flags r-r0)
  ))

(defun branch (instr reg)
  (let ((pc-offset (sign-extend (logand instr #x1FF) 9))
        (cond-flag (logand (ash instr -9) #x07)))

    (if (not (eq (logand cond-flag (aref reg R_COND)) 0))
      (incf (aref reg R_PC) pc-offset)
    )
  ))

(defun jump (instr reg)
  (let ((r-r1 (logand (ash instr -6) #x07)))
    (setf (aref reg R_PC) (aref reg r-r1))
  ))


(defun jump-register (instr reg)
  (let ((long-flag (logand (ash instr -11) 1)))
    (progn
      (setf (aref reg R7) (aref reg R_PC))
      (if (eq long-flag 1)
        (let ((long-pc-offset (sign-extend (logand instr #x7FF) 11)))
          (incf (aref reg R_PC) long-pc-offset)
        )
        (let ((r-r1 (logand (ash instr -6) #x07)))
          (setf (aref reg R_PC) (aref reg r-r1))
        )
      )
    ))
  )

(defun load-instr (instr reg)
  (let ((r-r0 (logand (ash instr -9) #x07))
       (pc-offset (sign-extend (logand instr #x1FF) 9)))
       (progn
         (setf (aref reg r-r0) (mem-read (+ (aref reg R_PC) pc-offset)))
         (update-flags r-r0)
       )
  ))

(defun load-register (instr reg)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (r-r1 (logand (ash instr -6) #x07))
        (offset (sign-extend (logand instr #x3F) 6)))

        (progn
          (setf (aref reg r-r0) (mem-read (+ (aref reg r-r1) offset)))
          (update-flags r-r0)
        )
  ))

(defun load-effective-address (instr reg)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (pc-offset (sign-extend (logand instr #x1FF) 9)))
    (progn
      (setf (aref reg r-r0) (+ (aref reg R_PC) pc-offset))
      (update-flags r-r0)
    ))
  )


(defun store (instr reg)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (pc-offset (sign-extend (logand instr #x1FF) 9)))
      (mem-write (+ (aref reg R_PC) pc-offset) (aref reg r-r0))
  ))

(defun store-indirect (instr reg)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (pc-offset (sign-extend (logand instr #x1FF) 9)))
      (mem-write (mem-read (+ (aref reg R_PC) pc-offset)) (aref reg r-r0))
  ))

(defun store-register (instr)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (r-r1 (logand (ash instr -6) #x07))
        (offset (sign-extend (logand instr #x3F) 6)))
    (mem-write (+ (aref reg r-r1) offset) (aref reg r-r0))
  ))

(defun trap (instr reg)
  (setf (aref reg R7) (aref reg R_PC))
  (let ((code (logand instr #xFF)))
    (cond ((eq code TRAP_GETC) (trap-get-c))
          ((eq code TRAP_OUT) (trap-out))
          ((eq code TRAP_PUTS) (trap-puts))
          ((eq code TRAP_IN) (trap-in))
          ((eq code TRAP_PUTSP) (trap-putsp))
          ((eq code TRAP_HALT) (trap-halt))
      )
  ))

(defun trap-get-c (reg)
  (let ((c (get-c)))
    (setf (aref reg R0) (the (unsigned-byte 16) c))
    (update-flags R0)
  )
)

(defun trap-out (reg)
  (put-c (the (signed-byte 8) (aref reg R0)))
)

(defun trap-in (reg)
  (progn
    (format t "Enter a character: ")
    (let ((c (get-c)))
      (put-c (the (signed-byte 8) c))
      (setf (aref reg R0) (the (unsigned-byte 16) c))
      (update-flags R0)
    )
  ))

(defun trap-puts (reg memory)
  (let ((c (aref reg R0)))
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
  (format t "HALT")
  (setf *running* nil))


(defun read-image-file (file memory)
  (let* ((origin (read-origin file))
         (max-read (- *memory-max* origin))
         (ptr origin)
         (read (read-sequence memory file :start origin :end max-read))
        )
    (loop while (> (decf read) 0) do
        (progn
          (mem-write ptr (swap16 (aref memory ptr)))
          (incf ptr)
        )
    )
  ))

(defun read-origin (file)
  (let ((origin-bytes (make-array 1 :element-type '(unsigned-byte 16))))
    (read-sequence origin-bytes file)
    (swap16 (aref origin-bytes 0))
  )) 


(defun swap16 (value)
  (progn
  (logior (logand (ash value 8) #xFF00) (ash value -8))))


(defun read-image (image-path memory)
  "Reads file at the provided path into the provided memory"
  (progn
    (setf image-stream (open image-path :direction :input :element-type '(unsigned-byte 16)))
    (read-image-file image-stream memory)
    (close image-stream))
  )
