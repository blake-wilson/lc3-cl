(declaim (optimize (debug 3)))
(load "memory.lisp")

(defparameter *running* t)

(defun run-vm ()
  (disable-input-buffering)
  (let ((args *posix-argv*))
    (loop for arg in args do
          (format t "~A" arg)
    )
    (if (< (length args) 2)
      (progn
        (format t "No file provided. Provide the filename of an executable object file~%")
        (exit)
      )
    )
    (read-image (nth 1 args))
  )
  ; since exactly one condition flag should be set at any given time, set the Z flag
  (setf (aref *reg* R_COND) FL_ZRO)

  ; set the PC to starting position
  ; 0x3000 is the default
  (setf (aref *reg* R_PC) #x3000)

  (loop while *running*
    do (let* (
             (instr (mem-read (aref *reg* R_PC)))
             (op (fetch-instruction instr))
          )
         (incf (aref *reg* R_PC) 1)
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

(defun fetch-instruction (instr)
    (ash instr -12))

(defun sign-extend (uint bits)
  "Sign extend to 16 bits"
  (if (= (ldb (byte 1 (1- bits)) uint) 0)
      uint
      (1+ (logxor (ash #xFFFF (+ -16 bits)) (- uint)))))

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
    (if (eq imm 1)
      (let ((imm5 (sign-extend (logand instr #x1F) 5)))
        (setf (aref *reg* r-r0) (mod (+ (aref *reg* r-r1) imm5) (ash 1 16)))
      )
      (let ((r-r2 (logand instr #x07)))
        (setf (aref *reg* r-r0) (mod (+ (aref *reg* r-r1) (aref *reg* r-r2)) (ash 1 16)))
      ))
    (update-flags r-r0)
    ))

(defun load-indirect (instr)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (pc-offset (sign-extend (logand instr #x1FF) 9)))
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
      (update-flags r-r0)
    ))

(defun not-instr (instr)
  (let ((r-r0 (logand (ash instr -9) #x07))
        (r-r1 (logand (ash instr -6) #x07)))

        (setf (aref *reg* r-r0) (logxor #xFFFF (aref *reg* r-r1)))
        (update-flags r-r0)
  ))

(defun branch (instr)
  (let ((pc-offset (sign-extend (logand instr #x1FF) 9))
        (cond-flag (logand (ash instr -9) #x07)))

    (if (not (eq (logand cond-flag (aref *reg* R_COND)) 0))
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
  (setf (aref *reg* R7) (aref *reg* R_PC))
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
  (let ((c (get-c)))
    (setf (aref *reg* R0) (the (unsigned-byte 16) c))
    (update-flags R0)
  )
)

(defun trap-out ()
  (put-c (the (signed-byte 8) (aref *reg* R0)))
)

(defun trap-in ()
  (progn
    (format t "Enter a character: ")
    (let ((c (get-c)))
      (put-c (the (signed-byte 8) c))
      (setf (aref *reg* R0) (the (unsigned-byte 16) c))
      (update-flags R0)
    )
  ))

(defun trap-puts ()
  (let ((c (aref *reg* R0)))
    (loop while (not (eq (aref *memory* c) #x0000)) do
      (progn
        (put-c (the (signed-byte 8) (logand (aref *memory* c) #xff)))
        (incf c)
      )
    )
  ))

(defun trap-putsp ()
  (let ((c (aref *memory* R0)))
    (loop while (not (eq (aref *memory* c) #x00)) do
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


(defun read-image-file (file)
  (let* ((origin (read-origin file))
         (max-read (- *memory-max* origin))
         (ptr origin)
         (read (read-sequence *memory* file :start origin :end max-read))
        )
    (loop while (> (decf read) 0) do
        (progn
          (mem-write ptr (swap16 (aref *memory* ptr)))
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


(defun read-image (image-path)
  (progn
    (setf image-stream (open image-path :direction :input :element-type '(unsigned-byte 16)))
    (read-image-file image-stream)
    (close image-stream))
  )
