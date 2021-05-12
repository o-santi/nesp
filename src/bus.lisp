(defstruct bus
  (cpu (6502:make-cpu))
  (ram (bytememory 2048))
  (ppu (make-ppu))
  cartridge
  (clock-counter 0))

(defun read-memory (bus address)
  (cond
    ((between address 0 #x1FFF) (aref (bus-ram bus) (logand address #x07FF)))
    ((between address #x2000 #x3FFF) (aref (bus-ppu-bus bus) (logand address #x0007)))))

(defun write-memory (bus address data)
   (cond 
    ((between address 0 #x1FFF) (setf (aref (bus-ram bus) (logand address #x07FF)) data))
    ((between address #x2000 #x3FFF) (setf (aref (bus-ppu-bus bus) (logand address #x07FF)) data))))

(defmethod clock ((bus obj))
  (clock (bus-ppu obj))
  (when (eq (mod (bus-clock-counter obj) 3) 0)
    (clock (bus-cpu obj)))
  (+1 (bus-clock-counter obj))
  (when (ppu-nmi (bus-ppu obj))
    (setf (ppu-nmi (bus-ppu obj)) nil)
    (cl-6502:nmi (bus-cpu obj))

  )

(defun reset (bus)
  (reset (bus-cpu bus))


