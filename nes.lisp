;;;; nes.lisp

(in-package #:nes)

(declaim (type (simple-array u8 (2048)) *ram*))
(defparameter *ram* (bytememory 2048)
  "The ram used by NES cpu.")

(defparameter *nes* (make-instance 'bus)
  "The main memory bus (basically the whole NES)")

(defvar *screen-width* 256)
(defvar *screen-height* 240)

(defun start-emulation (filename)
  (insert-cartridge filename *nes*)
  (setf (bus-ram *nes*) *ram*)
  (setf (bus-cpu *nes*) cl-6502:*cpu*)
  (reset-component *nes*))

(defun run-frame (bus)
  (loop while (not (ppu-frame-complete (bus-ppu bus)))
	do (clock bus))
  (setf (ppu-frame-complete (bus-ppu bus)) nil))

(defparameter donkey "~/programacao/lisp/Donkey Kong (World) (Rev A).nes")

(defun main ()
  (let ((start 0))
    (declare (optimize (speed 3) (safety 0))
	     (type fixnum *screen-height* *screen-width*))
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "NesP - a lisP Nes emulator"
                           :w (* 4 *screen-width*)
                           :h (* 4 *screen-height*)
			   :flags '(:shown :resizable))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
	(setf (ppu-renderer (bus-ppu *nes*)) renderer)
	(sdl2-ffi.functions:sdl-render-set-logical-size renderer *screen-width* *screen-height*)
        (sdl2:with-event-loop (:method :poll)
          (:keyup
           (:keysym keysym)
           (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
             (sdl2:push-event :quit)))
          (:idle
	   ()
	   (setf start (sdl2:get-performance-counter))
	   (sdl2:render-clear renderer)
	   (get-controller-state *nes*)
	   (run-frame *nes*)
	   (sdl2:render-present renderer)
	   (format t "fps:~,1f~%" (/ (float (sdl2:get-performance-frequency)) (- (sdl2:get-performance-counter) start))))
	  (:quit
	   ()
	   (setf (ppu-renderer (bus-ppu *nes*)) nil)
	   t)))))))
