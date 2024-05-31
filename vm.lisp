(declaim (optimize (debug 3)))
(load "memory.lisp")

(defparameter *running* t)

(defun run-vm ()
  ;(disable-input-buffering)
  (progn
  ;(let ((args *posix-argv*))
  (let ((args (list 1)))
    ;(if (< (length args) 2)
    ;  (exit 2)
    ;)
    (loop for arg in args 
          ; do (read-image arg))
          do (read-image "test.obj"))
  )
  (format t "read image\n\n")
  ;(format t "image is ~A" *memory*)
  ; since exactly one condition flag should be set at any given time, set the Z flag
  (setf (aref *reg* R_COND) FL_ZRO)

  ; set the PC to starting position
  ; 0x3000 is the default
  (setf (aref *reg* R_PC) #x3000)


  (step (format t "running program\n\n\n"))
  (loop while *running*
    do (let* (
             (instr (mem-read (aref *reg* R_PC)))
             (op (fetch-instruction instr))
          )
         (format t "instr ~A op ~A PC ~A" instr op (aref *reg* R_PC))
         (cond 
               ((eq op OP_BR)  (trace :break  (branch instr)))
               ((eq op OP_ADD)  (trace :break (add instr)))
               ((eq op OP_LD)   (trace :break (load-instr instr)))
               ((eq op OP_ST)   (trace :break (store instr)))
               ((eq op OP_JSR)  (trace :break (jump-register instr)))
               ((eq op OP_AND)  (trace :break (bitwise-and instr)))
               ((eq op OP_LDR)  (trace :break (load-register instr)))
               ((eq op OP_STR)  (trace :break (store-register instr)))
               ((eq op OP_RTI)  (trace :break ()))
               ((eq op OP_NOT)  (trace :break (not-instr instr)))
               ((eq op OP_LDI)  (trace :break (load-indirect instr)))
               ((eq op OP_STI)  (trace :break (store-indirect instr)))
               ((eq op OP_JMP)  (trace :break (jump instr)))
               ((eq op OP_RES)  (trace :break ()))
               ((eq op OP_LEA)  (trace :break (load-effective-address instr)))
               ((eq op OP_TRAP) (trace :break (trap instr)))
               (t (format t "unrecognized instruction ~A" instr))
          )
          (incf (aref *reg* R_PC) 1)
       )
   )
  )
)

(defun fetch-instruction (instr)
    ;(format t "instr is ~A" instr)
    (ash instr -12))

(defun sign-extend (x bitcount)
  (if (eq
        (logand (ash x (* (- bitcount 1) -1)) #x01) 1
      )
      (logior x (ash #xFFFF bitcount))
      x
  ))

(defun update-flags (r)
  (cond
    ((eq (aref *reg* r) 0)
      (setf (aref *reg* R_COND) FL_ZRO))
    ((eq (logand 1 (ash (aref *reg* r) -15)) 1)
      (setf (aref *reg* R_COND) FL_NEG))
    (t
      (setf (aref *reg* R_COND) FL_POS))
  ))

(defun add (instr)
  ; destination register
  (let ((r-r0 (logand (ash instr -9) #x07))
        (r-r1 (logand (ash instr -6) #x07))
        (imm (logand (ash instr -5) #x01))
       )
    (if imm
      (let ((imm5 (sign-extend (logand instr #x1F) 5)))
        (setf (aref *reg* r-r0) (+ (aref *reg* r-r1) imm5))
      )
      (let ((r-r2 (logand instr #x07)))
        (setf (aref *reg* r-r0) (+ (aref *reg* r-r1) (aref *reg* r-r2)))
      ))
    ))

(defun load-indirect (instr)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (pc-offset (sign-extend (logand instr #x1FF) -9)))
       (progn
         (setf (aref *reg* r-r0)
               (mem-read (mem-read (+ (aref *reg* R_PC) pc-offset))))
         (update-flags r-r0)
       )
    )
  )

(defun bitwise-and (instr)
  (let ((r-r0 (logand (ash instr -9) #x07))
      (r-r1 (logand (ash instr -6) #x07))
      (imm-flag (logand (ash instr 5) #x01)))
      (if (eq imm-flag 1)
        (let ((imm5 (sign-extend (logand instr #x1F) 5)))
          (setf (aref *reg* r-r0) (logand (aref *reg* r-r1) imm5))
        )
        (let ((r-r2 (logand instr #x07)))
          (setf (aref *reg* r-r0) (logand (aref *reg* r-r1) r-r2))
        )
      )
    ))

(defun not-instr (instr)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (r-r1 (logand (ash instr -6) #x07)))

        (setf (aref *reg* r-r0) (not (aref *reg* r-r1)))
        (update-flags r-r0)
  ))

(defun branch (instr)
  (let ((pc-offset (sign-extend (logand instr #x1FF) 9))
        (cond-flag (logand (ash instr -9) #x07)))

    (if (eq (logand cond-flag (aref *reg* R_COND)) 1)
      (incf (aref *reg* R_PC) pc-offset)
    )
  ))

(defun jump (instr)
  (let ((r-r1 (logand (ash instr -6) #x07)))
    (setf (aref *reg* R_PC) (aref *reg* r-r1))
  ))


(defun jump-register (instr)
  (let ((long-flag (logand (ash instr -11) 1)))
    (progn
      (setf (aref *reg* R7) (aref *reg* R_PC))
      (if (eq long-flag 1)
        (let ((long-pc-offset (sign-extend (logand instr #x7FF) 11)))
          (incf (aref *reg* R_PC) long-pc-offset)
        )
        (let ((r-r1 (logand (ash instr -6) #x07)))
          (setf (aref *reg* R_PC) (aref *reg* r-r1))
        )
      )
    ))
  )

(defun load-instr (instr)
  (let ((r-r0 (logand (ash instr -9) #x07))
       (pc-offset (sign-extend (logand instr #x1FF) 9)))
       (progn
         (setf (aref *reg* r-r0) (mem-read (+ (aref *reg* R_PC) pc-offset)))
         (update-flags r-r0)
       )
  ))

(defun load-register (instr)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (r-r1 (logand (ash instr -6) #x07))
        (offset (sign-extend (logand instr #x3F) 6)))

        (progn
          (setf (aref *reg* r-r0) (mem-read (+ (aref *reg* r-r1) offset)))
          (update-flags r-r0)
        )
  ))

(defun load-effective-address (instr)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (pc-offset (sign-extend (logand instr #x1FF) 9)))
    (progn
      (setf (aref *reg* r-r0) (+ (aref *reg* R_PC) pc-offset))
      (update-flags r-r0)
    ))
  )


(defun store (instr)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (pc-offset (sign-extend (logand instr #x1FF) 9)))
      (mem-write (+ (aref *reg* R_PC) pc-offset) (aref *reg* r-r0))
  ))

(defun store-indirect (instr)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (pc-offset (sign-extend (logand instr #x1FF) 9)))
      (mem-write (mem-read (+ (aref *reg* R_PC) pc-offset)) (aref *reg* r-r0))
  ))

(defun store-register (instr)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (r-r1 (logand (ash instr -6) #x07))
        (offset (sign-extend (logand instr #x3F) 6)))
    (mem-write (+ (aref *reg* r-r1) offset) (aref *reg* r-r0))
  ))

(defun trap (instr)
  (let ((code (logand instr #xFF)))
    (cond ((eq code TRAP_GETC) (trap-get-c))
          ((eq code TRAP_OUT) (trap-out))
          ((eq code TRAP_PUTS) (trap-puts))
          ((eq code TRAP_IN) (trap-in))
          ((eq code TRAP_PUTSP) (trap-putsp))
          ((eq code TRAP_HALT) (trap-halt))
      )
  ))

(defun trap-get-c ()
  (let ((c (aref *reg* R0)))
    (loop while (not (eq (aref *memory* c) #x00)) do
        (format t (aref *memory* c))
        (incf c)
    )
  ))

(defun trap-get-c ()
  (let ((c (read-char)))
    (setf (aref *reg* R0) c)
    (update-flags R0)
  )
)

(defun trap-out ()
  (write-char (aref *reg* R0))
  (format t "writing ~A" (aref *reg* R0))
  (finish-output)
)

(defun trap-in ()
  (progn
    (format t "Enter a character: ")
    (let ((c (read-char)))
      (write-char c)
      (finish-output)
      (setf (aref *reg* R0) c)
      (update-flags R0)
    )
  ))

(defun trap-puts ()
  (let ((c (aref *reg* R0)))
    (loop while (not (eq (aref *memory* c) #x00)) do
      (progn
        (write-char (aref *memory* c))
        (incf c)
      )
    )
    (finish-output)
  ))

(defun trap-putsp ()
  (let ((c (aref *memory* R0)))
    (loop while (not (eq (aref *memory* c) #x00)) do
      (let ((char1 (logand c #xff))
            (char2 (ash c -8)))
            (write-char char1)
            (if (not (eq 0 char2))
              (write-char char2)
            )
      )
      (incf c)
    )
    (finish-output)
  ))


(defun trap-halt ()
  (format t "HALT")
  (setf *running* nil))


(defun read-image-file (file)
  (let* ((origin (read-origin file))
         (max-read (- *memory-max* origin))
         (ptr origin)
         (read (read-sequence *memory* file :start origin :end max-read))
        )
    (format t "origin is ~A\n\n" origin)
    (loop while (> (decf read) 0) do
        (progn
          (mem-write ptr (swap16 ptr))
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
    (format t "Value is ~A\n" value)
  (logior (logand (ash value 8) #xFF00) (ash value -8))))


(defun read-image (image-path)
  (progn
    (setf image-stream (open image-path :direction :input :element-type '(unsigned-byte 16)))
    (read-image-file image-stream)
    (close image-stream))
  )
