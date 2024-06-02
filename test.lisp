(declaim (optimize (debug 3)))
(load "vm.lisp")

; load memory with test data
(read-image "test.obj")

; origin should be 0x3000
(eq (aref *memory* #x3000) #x2c14)
(eq (aref *memory* #x3001) #xea15)

(setf (aref *reg* R0) 5)
(setf (aref *reg* R1) 10)
; 0001 | 010 (dr = 2) | 000 (sr = 0) | 0 | 00 | 001 (sr = 1)
(defparameter *instr* #b0001010000000001)
(setf (aref *reg* 0) 100)
(setf (aref *reg* 1) 200)

(add *instr*)
(aref *reg* 2)

; branch instr
; | 0000 | 0 0 1 | 000000001
;   instr| cond |  pc-offset
(setf *instr* #b0000001000000001)
(setf (aref *reg* R_COND) #b001)
(setf (aref *reg* R_PC) 100)
(branch *instr*)
(eq (aref *reg* R_PC) 101)

; Store instr
; 0011 | 011 | 011111111
; instr | reg | pc-offset
(setf (aref *reg* 3) #xBA)
(setf *instr* #b0011011011111111)
(setf (aref *reg* R_PC) 100)
(store *instr*)
(eq (aref *memory* (+ 100 255)) #xBA)


; load-effective-address
; 1110 | 001     | 000000010
; instr| dst-reg | pc-offset
(setf *instr* #b1110001000000010)
(setf (aref *reg* R_PC) #xF0)
(load-effective-address *instr*)
(eq (aref *reg* 1) (+ #xF0 2))

(load-effective-address #xea15)

(eq (sign-extend -1 5) #x1111)

; Store indirect instr
; 1011 | 011 | 011111111
; instr | reg | pc-offset
(setf (aref *reg* R_PC) 100)
(setf (aref *memory* (+ 100 255)) 500)
(setf (aref *reg* 3) #xBC)
(setf *instr* #b0011011011111111)
(setf (aref *reg* R_PC) 100)
(store-indirect *instr*)
(eq (aref *memory* 500) #xBC)


; load-register instr
; 0110 | 011      | 100      | 011111 
; instr | dst-reg | base reg | pc-offset
(setf (aref *reg* 4) #xBA)
(setf *instr* #b0110011100011111)
(setf (aref *reg* R_PC) 100)
(setf (aref *memory* (+ #xBA 31)) 444)
(load-register *instr*)
(eq (aref *reg* 3) 444)

; store-indirect instr
; 0111 | 011      | 100      | 011111 
; instr | src-reg | base reg | pc-offset
(setf (aref *reg* 3) 333)
(setf (aref *reg* 4) #xBA)
(setf *instr* #b0111011100011111)
(store-register *instr*)
(eq (aref *memory* (+ 31 #xBA)) 333)

; 0111 000 ; 110; 111111
; (setf (aref *reg* 0) 200)
; (setf (aref *reg* 5) 10)
; (setf *instr* #b0111000110111111)
; (store-register *instr*)
; (eq (aref *memory* (+ 10 -1)) 200)

; jump-register
; 0100 | 1 | 00000000110
; instr| c | pc-offset
(setf *instr* #b0100100000000110)
(setf (aref *reg* R_PC) 100)
(jump-register *instr*)
(eq (aref *reg* R_PC) 106)

; jump-register 2; jsrr
; 0100 | 0 | 00 | 010      | 000000
; instr| c | ph | base-reg | ph

(setf *instr* #b0100000010000000)
(setf (aref *reg* 2) 30)
(jump-register *instr*)
(eq (aref *reg* R_PC) 30)



; 1111 | 0000 | 00100010
; 1111000000100010
(setf *instr* #b1111000000100010)
(trap *instr*)



; load-indirect 40969
; 1010 | dst-reg | pc-offset
; 1010 | 000 | 0000001001
                ; reg: (0 67427368 0 0 0 12310 61473 13018 2 13075)
; reg: (0 3019682 0 0 0 12314 16380 12938 2 12995)
(setf *instr* #b1010000000001001)
(setf (aref *reg* R0) 0)
(setf (aref *reg* R_PC) 12995)
(setf (aref *memory* (+ 12995 9)) 65024)
(setf (aref *memory* 65024) 200)
(load-indirect *instr*)

(eq (aref *reg* R0) 200)

; not
; instr|dst-reg | src-reg| 1 | 11111
; 1001 | 000    | 001    | 1 | 11111
(setf *instr* #b1001000001111111)
(setf (aref *reg* R1) #b1110010011001101)
(not-instr *instr*)
(eq (aref *reg* R0) #b0001101100110010)
