(in-package :cl-user)

(defpackage :mapper-creator
  (:use :common-lisp)
  (:export :mapper
	   :mapper->cpuRead
	   :mapper->cpuWrite
	   :mapper->ppuRead
	   :mapper->ppuWrite))

(defclass mapper ()
  ((id :accessor id)
   (pgr-banks :accessor pgr-banks)
   (chr-banks :accessor chr-banks)))

(defun between (first second third)
  (and (>= first second)
       (<= first third)))

(defgeneric mapper->cpuRead (mapper-id cpu addr)
  (:documentation "Generic function for read cpu data"))
      
(defgeneric mapper->cpuWrite (mapper-id cpu addr data)
  (:documentation "Generic function for write cpu data"))

(defgeneric mapper->ppuRead (mapper-id ppu addr)
  (:documentation "Generic function for read ppu data"))

(defgeneric mapper->ppuWrite (mapper-id ppu addr data)
  (:documentation "Generic function for write ppu data"))

;; i need a way to differentiate between mapper types
;; thought of abstract classes but doesnt really make sense in lisp
;; maybe a parent class and every type derives from it
;; do not know yet, will decide later

(defmethod mapper->cpuRead ((mapper-0 cpu addr)
  (when (between addr #x8000 #xFFFF)
    (logiand addr (if (> (pgr-banks mapper-0) 1) #x7FFF #x3FFF))))

(defmethod mapper->ppuRead (mapper-0 ppu addr)
  (when (between addr #x0000 #x1FFF)
    addr))

