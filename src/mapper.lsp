(defclass mapper ()
  ((id :accessor id)
   (pgr-banks :accessor pgr-banks)
   (chr-banks :accessor chr-banks)))

(defun between (first second third)
  (and (>= first second)
       (<= first third)))

(defgeneric mapper-cpuRead (mapper cpu addr)
  (:documentation "Generic function for mapping cpu data reads"))
      
(defgeneric mapper-cpuWrite (mapper cpu addr)
  (:documentation "Generic function for mapping cpu data writes"))

(defgeneric mapper-ppuRead (mapper ppu addr)
  (:documentation "Generic function for mapping ppu data reads"))

(defgeneric mapper-ppuWrite (mapper ppu addr)
  (:documentation "Generic function for mapping ppu data writes"))

;; all a mapper does is get an address as an input
;; and returns another address as an output, where the cpu/ppu should
;; read or write

;; i need a way to differentiate between mapper types
;; thought of abstract classes but doesnt really make sense in lisp
;; maybe a parent class and every type derives from it
;; do not know yet, will decide later

(defclass mapper-0 (mapper)
  ())

(defmethod mapper-cpuRead ((mapper mapper-0) addr)
  (when (between addr #x8000 #xFFFF)
    (logiand addr (if (> (pgr-banks mapper-0) 1) #x7FFF #x3FFF))))

(defmethod mapper-ppuRead ((mapper mapper-0) addr)
  (when (between addr #x0000 #x1FFF)
    addr))

(defmethod mapper-cpuWrite ((mapper mapper-0) addr)
  (when (between addr #x0000 #x1FFF) adrr))

(defmethod mapper-ppuWrite ((mapper mapper-0) addr)
  ())
