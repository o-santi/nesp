;;;; nes.lisp

(in-package #:nes)

(declaim (type (simple-array u8 (2048)) *ram*))
(defparameter *ram* (bytememory 2048)
  "The ram used by NES cpu.")

(defparameter *nes* (make-instance 'bus)
  "The main memory bus (basically the whole NES)")

(defvar *screen-width* (* 4 256))
(defvar *screen-height* (* 4 240))

(defun start-emulation (filename)
  (insert-cartridge filename *nes*)
  (setf (bus-ram *nes*) *ram*)
  (setf (bus-cpu *nes*) cl-6502:*cpu*)
  (reset-component *nes*))

(defun run-frame (bus)
  (loop while (not (ppu-frame-complete (bus-ppu bus)))
	do (clock bus))
  (setf (ppu-frame-complete (bus-ppu bus)) nil))

(defun draw-pixels (renderer)
  (loop for func across *pixels*
	do (progn
	     (funcall (first func) renderer)
	     (funcall (second func) renderer)))
  (setf *pixels* (make-array 0 :element-type 'function :adjustable t :fill-pointer 0)))

(defparameter donkey "~/programacao/lisp/Donkey Kong (World) (Rev A).nes")

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "NesP - a lisP Nes emulator"
                           :w *screen-width*
                           :h *screen-height*
			   :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keyup
           (:keysym keysym)
           (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
             (sdl2:push-event :quit)))
          (:idle ()
	   (get-controller-state *nes*)
           (run-frame *nes*)
	   (draw-pixels renderer)
	   (sdl2:render-present renderer)
	   (sdl2:delay 16))
          (:quit () t))))))
