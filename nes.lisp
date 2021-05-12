;;;; nes.lisp

(in-package #:nes)

(defparameter *ram* (bytememory 2048)
  "The ram used by NES cpu.")

(defparameter *ppu-bus* (bytememory 2048)
  "The ram used by NES cpu.")

(defparameter *bus* (make-bus)
  "The main memory bus (basically the whole NES)")

;;(defun load-cartridge (filename)
;;  (setf (bus-cartridge bus) (parse-cartridge filename)))


