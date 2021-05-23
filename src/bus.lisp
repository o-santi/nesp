(in-package #:nes)

(defstruct bus
  ram
  cpu
  (ppu (make-instance 'ppu))
  cartridge
  (controller (make-array 2 :element-type 'u8 :initial-element 0))
  (controller-state (make-array 2 :element-type 'u8 :initial-element 0))
  (clock-counter 0))

(defun read/bus->cpu (bus address)
  (with-slots (controller-state ram cpu ppu cartridge) bus
    (cond
      ((read/cart->cpu cartridge address) (read/cart->cpu cartridge address)) ;; this is SO dumb
      ((between address 0 #x1FFF) (aref ram (logand address #x07FF)))
      ((between address #x2000 #x3FFF) (read/ppu->cpu ppu (logand address #x0007)))
      ((between address #x4016 #x4017) ;; controllers
       (let* ((array-index (logand address #x01))
	      (return-data (if (logbitp 7 (aref controller-state array-index)) 1 0)))
	 (setf (aref controller-state array-index) (logand (ash (aref controller-state array-index) 1) #xFF))
	 return-data))
      (t 0))))

(defun write/bus->cpu (bus address data)
  (with-slots (ram cpu ppu cartridge controller controller-state) bus
    (cond
      ((write/cart->cpu cartridge address data)) ;;cartridge 
      ((between address 0 #x1FFF) (setf (aref ram (logand address #x07FF)) data)) ;; cpu ram
      ((between address #x2000 #x3FFF) (write/ppu->cpu ppu (logand address #x0007) data)) ;; ppu register
      ((between address #x4016 #x4017) ;; controllers
       (let ((array-index (logand address #x01)))
	 (setf (aref controller-state array-index) (aref controller array-index)))))))

(defun clock-cpu (bus)
  (with-slots (cpu cpu-cycles clock-counter) bus
   (when (eq (6502:cpu-cc cpu) 0)
     (let ((opcode (read/bus->cpu bus (6502:cpu-pc cpu))))
       ;;(current-instruction (bus-cpu bus) t)
       (6502:step-cpu cpu opcode)))
    (decf (6502:cpu-cc cpu))))
  
(defun clock (bus)
  (with-slots (cpu ppu clock-counter) bus
  
    (clock-ppu ppu)
  
    (when (eq (mod clock-counter 3) 0)
      (clock-cpu bus))
    
    (when (eq (ppu-nmi-bit ppu) 1)
      (setf (ppu-nmi-bit ppu) nil)
      (6502:nmi cpu))
    
    (incf clock-counter)))

(defmethod reset-component ((obj bus))
  (with-slots (ppu cartridge clock-counter) obj
    (reset-cpu obj)
    (reset-component cartridge)
    (reset-component ppu)
    (setf clock-counter 0)))

(defun reset-cpu (bus)
  (with-slots (cpu cpu-cycles) bus
    (let* ((absolute-addr #xFFFC)
	   (lo (read/bus->cpu bus absolute-addr))
 	   (hi (read/bus->cpu bus (1+ absolute-addr))))
      (setf (6502:cpu-pc cpu) (logior (ash hi 8) lo))
      (setf (6502:cpu-ar cpu) 0)
      (setf (6502:cpu-xr cpu) 0)
      (setf (6502:cpu-yr cpu) 0)
      (setf (6502:cpu-sp cpu) #xfd)
      (setf (6502:cpu-sr cpu) #x24)
      (setf (6502:cpu-cc cpu) 8))))

(defun insert-cartridge (filename bus)
  (let ((cartridge (parse-cartridge filename)))
    (setf (bus-cartridge bus) cartridge)
    (insert-cartridge-in-ppu cartridge (bus-ppu bus))))

(defmacro check-key-up (key-scancode position)
  `(setf (ldb (byte 1 ,position) (aref (bus-controller bus) 0)) (if (sdl2:keyboard-state-p ,key-scancode) 1 0)))

(defun get-controller-state (bus)
  (setf (aref (bus-controller bus) 0) 0)
  (when (sdl2:keyboard-state-p :scancode-r) (reset-component *nes*))
  (check-key-up :scancode-v 7) ;; A
  (check-key-up :scancode-c 6) ;; B
  (check-key-up :scancode-x 5) ;; select
  (check-key-up :scancode-z 4) ;; start
  (check-key-up :scancode-w 3) ;; up key
  (check-key-up :scancode-s 2) ;; down key
  (check-key-up :scancode-a 1) ;; left key
  (check-key-up :scancode-d 0)) ;; right key 
  
  
  
(defun execute-instruction (bus)
  (loop while (not (eql (6502:cpu-cc (bus-cpu bus)) 0))
 	do (clock bus))
  (dotimes (i 3)
    (clock bus))
  (current-instruction (bus-cpu bus) t)
  (bus-cpu bus))

(defun 6502:get-range (start finish)
  (let* ((length (- finish start))
	 (return-memory (bytememory length)))
    (dotimes (i length)
      (setf (aref return-memory i) (read/bus->cpu *nes* (+ start i))))
    return-memory))

(defun (setf 6502:get-range) (bytevector start)
  (loop for i from 0 and x across bytevector
	do (write/bus->cpu *nes* (+ start i) x)))

(defun 6502:get-byte (index)
  (read/bus->cpu *nes* index))

(defun (setf 6502:get-byte) (new-val address)
  (write/bus->cpu *nes* address new-val))



