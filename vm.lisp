(defun run-vm
  (let ((args (sb-ext:*posix-argv*)))
    (if (< (length args) 2)
      (exit 2)
    )
    (loop for arg in args 
          do (read-image arg))
  )
  ; since exactly one condition flag should be set at any given time, set the Z flag
  (setf (aref *reg* R_COND) FL_ZERO)

  ; set the PC to starting position
  ; 0x3000 is the default
  (setf (aref *reg* R_PC) #x3000)


  (let ((running) t)
    (loop while running
      do (let ((op (fetch-instruction)))
           (cond ((eq op :OP_ADD) ())
                 ((eq op :OP_LD) ())
                 ((eq op :OP_ST) ())
                 ((eq op :OP_JSR) ())
                 ((eq op :OP_AND) ())
                 ((eq op :OP_LDR) ())
                 ((eq op :OP_STR) ())
                 ((eq op :OP_RTI) ())
                 ((eq op :OP_NOT) ())
                 ((eq op :OP_LDI) ())
                 ((eq op :OP_STI) ())
                 ((eq op :OP_JMP) ())
                 ((eq op :OP_RES) ())
                 ((eq op :OP_LEA) ())
            )
         )
        )
     )
)

(defun fetch-intsruction ()
  (let ((instr (mem-read (R_PC))))
    (ash instr -12)
  ))

(defun sign-extend (x bitcount)
  (if (eq
        (logand (ash (- bitcount 1)) 1)
      )
      (logor x (ash #xFFFF bitcount))
      x
  ))

(defun update-flags (r)
  (cond
    ((eq (aref *registers* r) 0)
      (setf (aref *registers* R_COND) FL_ZERO))
    ((eq (logand 1 (ash (aref *registers* r) 15)) 1)
      (setf (aref *registers* R_COND) FL_NEG))
    (t
      (setf (aref *registers* R_COND) FL_POS))
  ))

(defun add (instr)
  ; destination register
  (let ((r0 (logand (ash instr 9) #x07))
        (r1 (logand (ash instr 6) #x07))
        (imm (logand (ash instr 5) #x01))
       )
    (if imm
      (let ((imm5 (sign-extend (logand instr #x1F) 5)))
        (setf (aref *reg* r0) (+ (aref *reg* r1) imm5))
      )
      (let ((r2 (logand instr #x07)))
        (setf (aref *reg* r0) (+ (aref *reg* r0) (aref *reg* r2)))
      ))
    ))

(defun ldi (instr)
  (let ((r0 (ash instr #x09))
        (pc-offset (sign-extend (logand instr #x1FF) 9))
       )
       (progn
         (setf (aref *reg* r0)
               (mem-read (mem-read (+ (aref *reg* R_PC) pc-offset))))
         (update-flags r0)
       )
    )
  )
