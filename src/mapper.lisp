;; all a mapper does is get an address as an input
;; and returns another address as an output, where the cpu/ppu should
;; read or write

;; i need a way to differentiate between mapper types
;; thought of abstract classes but doesnt really make sense in lisp
;; maybe a parent class and every type derives from it
;; do not know yet, will decide later

(defclass mapper ()
  ((id :accessor id)
   (pgr-banks :accessor pgr-banks)
   (chr-banks :accessor chr-banks)))

(defclass mapper-000 (mapper)
  ())

(defgeneric map-read (mapper addr obj)
  (:documentation "Map addr to read inside obj"))

(defgeneric map-write (mmaper addr obj)
  (:documentation "Map addr to write data at addr inside obj"))

(defmethod map-read ((mapper mapper-000) addr (type obj (eql cl-6502:cpu)))
   (when (between addr #x8000 #xFFFF)
     (logiand addr (if (> (pgr-banks obj) 1) #x7FFF #x3FFF))))

(defmethod map-read ((mapper mapper-000) addr (type obj (eql ppu)))
  (when (between addr #x0000 #x1FFF)
    addr))

(defmethod map-write ((mapper mapper-000) addr (type obj (eql cl-6502:cpu)))
  (when (between addr #x0000 #x1FFF)
    addr))

(defmethod map-write ((mapper mapper-000) addr (type objx (eql ppu)))
  '())
