(in-package #:nes)

(defstruct bus
  ram
  cpu
  cartridge
  (ppu (make-instance 'ppu))
  (dma-page 0 :type u8)
  (dma-addr 0 :type u8)
  (dma-data 0 :type u8)
  (dma-transfer nil :type boolean)
  (dma-dummy nil :type boolean)
  (controller (make-array 2 :element-type 'u8 :initial-element 0))
  (controller-state (make-array 2 :element-type 'u8 :initial-element 0))
  (clock-counter 0 :type fixnum))

(declaim (ftype (function (bus u16)) read/bus->cpu))
(defun read/bus->cpu (bus address)
  (with-slots (controller-state ram cpu ppu cartridge) bus
    (declare (optimize (speed 3) (safety 0))
	     (type (simple-array u8) ram controller-state)
	     (values u8))
    (cond
      ((read/cart->cpu cartridge address) (read/cart->cpu cartridge address)) ;; this is SO dumb
      ((between address 0 #x1FFF) (aref ram (logand address #x07FF)))
      ((between address #x2000 #x3FFF) (read/ppu->cpu ppu (logand address #x0007))) 
      ((between address #x4016 #x4017) ;; controllers
       (let* ((array-index (ldb (byte 1 0) address))
	      (return-data (ldb (byte 1 7) (aref controller-state array-index))))
	 (setf (aref controller-state array-index) (logand (ash (aref controller-state array-index) 1) #xFF))
	 return-data))
      (t 0))))

(declaim (ftype (function (bus u16 u8)) write/bus->cpu))
(defun write/bus->cpu (bus address data)
  (with-slots (ram cpu ppu cartridge controller controller-state dma-page dma-addr dma-transfer) bus
    (declare (optimize (speed 3) (safety 0))
	     (type (simple-array u8) ram controller-state controller))
    (cond
      ((write/cart->cpu cartridge address data)) ;;cartridge 
      ((between address 0 #x1FFF) (setf (aref ram (logand address #x07FF)) data)) ;; cpu ram
      ((between address #x2000 #x3FFF) (write/ppu->cpu ppu (logand address #x0007) data)) ;; ppu register
      ((eql address #x4014)
       (progn
	 (setf dma-page data)
	 (setf dma-addr 0)
	 (setf dma-transfer t)))
      ((between address #x4016 #x4017) ;; controllers
       (let ((array-index (logand address #x01)))
	 (setf (aref controller-state array-index) (aref controller array-index)))))))

(declaim (ftype (function (bus)) clock-cpu))
(defun clock-cpu (bus)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (cpu cpu-cycles clock-counter) bus
   (when (eq (6502:cpu-cc cpu) 0)
     (let ((opcode (read/bus->cpu bus (6502:cpu-pc cpu))))
       ;(current-instruction (bus-cpu bus) t)
       (6502:step-cpu cpu opcode)
       ;(format t "~a~%" (bus-cpu bus))
       ))
    (decf (6502:cpu-cc cpu))))

(declaim (ftype (function (bus)) clock))
(defun clock (bus)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (cpu ppu clock-counter dma-page dma-transfer dma-dummy dma-data dma-addr) bus
    (clock-ppu ppu)
    (when (eq (mod clock-counter 3) 0)
      (if dma-transfer
	  (if dma-dummy
	      (when (eq (mod clock-counter 2) 1) (setf dma-dummy nil))
	      (if (eq (mod clock-counter 2) 0)
		  (setf dma-data (read/bus->cpu bus (logior (ash dma-page 8) dma-addr)))
		  (progn (write-oam ppu dma-addr dma-data)
			 (setf dma-addr (logand (1+ dma-addr) #xFF))
			 (when (eql dma-addr 0)
			   (setf dma-transfer nil)
			   (setf dma-dummy t)))))
	  (clock-cpu bus)))
    
    (when (ppu-nmi-bit ppu)
      (setf (ppu-nmi-bit ppu) nil)
      (6502:nmi cpu))
    
    (incf clock-counter)))

(defmethod reset-component ((obj bus))
  (with-slots (ppu cartridge clock-counter dma-page dma-addr dma-data dma-dummy dma-transfer) obj
    (reset-cpu obj)
    (reset-component cartridge)
    (reset-component ppu)
    (setf dma-page 0)
    (setf dma-addr 0)
    (setf dma-data 0)
    (setf dma-dummy t)
    (setf dma-transfer nil)
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
  `(setf (ldb (byte 1 ,position) (aref controller 0)) (if (sdl2:keyboard-state-p ,key-scancode) 1 0)))

(declaim (ftype (function (bus)) get-controller-state))
(defun get-controller-state (bus)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (controller) bus
    (declare (type (simple-array u8) controller))
    (setf (aref controller 0) 0)
    (when (sdl2:keyboard-state-p :scancode-r) (format t "reset nes") (terpri)(reset-component *nes*))
    (check-key-up :scancode-v 7) ;; A
    (check-key-up :scancode-c 6) ;; B
    (check-key-up :scancode-x 5) ;; select
    (check-key-up :scancode-z 4) ;; start
    (check-key-up :scancode-w 3) ;; up key
    (check-key-up :scancode-s 2) ;; down key
    (check-key-up :scancode-a 1) ;; left key
    (check-key-up :scancode-d 0))) ;; right key 
  
  
(defun execute-instruction (bus)
  (loop while (not (eql (6502:cpu-cc (bus-cpu bus)) 0))
 	do (clock bus))
  (dotimes (i 3)
    (clock bus))
  (current-instruction (bus-cpu bus) t)
  (bus-cpu bus))

(declaim (ftype (function (u16 u16) array) 6502:get-range))
(defun 6502:get-range (start finish)
  (declare (optimize (speed 3) (safety 0))
	   (values (simple-array u8)))
  (let* ((length (- finish start))
	 (return-memory (bytememory length)))
    (declare (type (simple-array u8) return-memory))
    (dotimes (i length)
      (setf (aref return-memory i) (read/bus->cpu *nes* (+ start i))))
    return-memory))

(declaim (ftype (function (array u16)) (setf 6502:get-range)))
(defun (setf 6502:get-range) (bytevector start)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array u16) bytevector))
  (loop-for-element-in bytevector
    (write/bus->cpu *nes* (+ start i) element)))

(declaim (ftype (function (u16) u8) 6502:get-byte))
(defun 6502:get-byte (address)
  (declare (optimize (speed 3) (safety 0))
	   (values u8))
  (read/bus->cpu *nes* address))

(declaim (ftype (function (u8 u16)) (setf 6502:get-byte)))
(defun (setf 6502:get-byte) (new-val address)
  (declare (optimize (speed 3) (safety 0)))
  (write/bus->cpu *nes* address new-val))

