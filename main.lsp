(in-package :cl-user)

(load "cartridge-parser.lsp")
(defpackage :lisp.nes
  (:use :common-lisp :binary-parser))

(ql:quickload 'cl-6502)

(defparameter *ram* (bytememory 2048)
  "The ram used by NES cpu.")

(defparameter *ppu-bus* (bytememory 2048)
  "The ram used by NES cpu.")

(defstruct ppu
  cartridge
  (name-table (bytememory 2048))
  (pallete-table (bytememory 32)))
	   
(defstruct bus
  (cpu (6502:make-cpu))
  (ram (bytememory 2048))
  (ppu (make-ppu))
  cartridge)

(defun between (first second third)
  (and (>= first second)
       (<= first third)))

(defun read-memory (bus address)
  (mapper->cpuRead (mapper))
  (cond
    ((between address 0 #x1FFF) (aref (bus-ram bus) (logand address #x07FF)))
    ((between address #x2000 #x3FFF) (aref (bus-ppu-bus bus) (logand address #x0007)))))

(defun write-memory (bus address data)
   (cond 
    ((between address 0 #x1FFF) (setf (aref (bus-ram bus) (logand address #x07FF)) data))
    ((between address #x2000 #x3FFF) (setf (aref (bus-ppu-bus bus) (logand address #x07FF)) data))))


(defun clock (bus)) ;; ToDo : DEFINE

(defun reset (bus)
  (reset *cpu*))


