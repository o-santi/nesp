;; all a mapper does is get an address as an input
;; and returns another address as an output, where the cpu/ppu should
;; read or write

;; i need a way to differentiate between mapper types
;; thought of abstract classes but doesnt really make sense in lisp
;; maybe a parent class and every type derives from it
;; do not know yet, will decide later

(in-package #:nes)

(defclass mapper ()
  ((id :accessor id)
   (pgr-banks :accessor pgr-banks)
   (chr-banks :accessor chr-banks)))

(defclass mapper-000 (mapper)
  ())

(defgeneric map-read (map addr obj)
  (:documentation "Map addr to read inside obj"))

(defgeneric map-write (map addr obj)
  (:documentation "Map addr to write data at addr inside obj"))

(defmethod map-read ((map mapper-000) addr (obj (eql :cpu)))
   (when (between addr #x8000 #xFFFF)
     (logand addr (if (> (pgr-banks map) 1) #x7FFF #x3FFF))))

(defmethod map-read ((map mapper-000) addr (obj (eql :ppu)))
  (when (between addr #x0000 #x1FFF)
    addr))

(defmethod map-write ((map mapper-000) addr (obj (eql :cpu)))
  (when (between addr #x8000 #xFFFF)
     (logand addr (if (> (pgr-banks map) 1) #x7FFF #x3FFF))))

(defmethod map-write ((map mapper-000) addr (obj (eql :ppu)))
  (when (and (between addr #x0000 #x1FFF)
	     (eql (chr-banks map) 0))
    addr))

(defmethod reset-component ((obj mapper-000))
  '())
