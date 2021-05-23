(in-package #:nes)

(defstruct cartridge
  game-file
  mapper 
  mirror
  header
  (prog-banks 0 :type u8)
  (char-banks 0 :type u8) 
  (v-prog (make-array 0 :fill-pointer 0 :adjustable t))
  (v-char (make-array 0 :fill-pointer 0 :adjustable t)))

(define-binary-class cartridge-header
    ((name (string :length 4)) ;bytes 0-3
     (pgr-rom-size u8) ;; byte 4
     (chr-rom-size u8) ;; bytfe 5
     (mapper1 u8) ;; byte 6
     (mapper2 u8) ;; byte 7
     (pgr-ram-size u8) ;;byte 8
     (tv-sys1 u8) ;; byte 9
     (tv-sys u8) ;; byte 10
     (unused (string :length 5)))) ;;bytes 11-15 are unused

(defun get-mapper-id (header)
  (logior (ash (ash (mapper2 header) -4) 4) (ash (mapper1 header) -4)))

;;(defun create-mapper (header)
;;  (let* ((id (get-mapper-id header))
;;	 (mapper (concatenate 'string "mapper-" (format nil "~3,'0D" id))))
;;    (make-instance (intern mapper))))
;; this is not working rn
;; TODO: make this work

(defun create-mapper (header)
  (let ((id (get-mapper-id header)))
    (case id
      (0 (make-instance 'mapper-000)))))

(defun parse-cartridge (nes-file)
  (let ((cart (make-cartridge)))
    (with-slots (game-file mapper mirror header prog-banks char-banks v-prog v-char) cart
      (with-open-file (filestream nes-file :element-type 'u8)
	(setf header (read-value 'cartridge-header filestream))
	(setf mapper (create-mapper header))
	(setf game-file nes-file)

	(setf prog-banks (pgr-rom-size header))
	(setf char-banks (chr-rom-size header))
	(setf (pgr-banks (cartridge-mapper cart)) prog-banks) 
	(setf (chr-banks (cartridge-mapper cart)) char-banks) 
	(when (logbitp 3 (mapper1 header))
	  (read-value 'string filestream :length 512)) ;; just junk, does not matter
	;;read into it
	(setf v-prog (read-value 'u8 filestream :length (* 16384 prog-banks)))
	;; read into it
	(setf v-char (read-value 'u8 filestream :length (* 8192 char-banks)))
	(setf mirror (if (eql (logand (mapper1 header) #x01) 1)
			 :vertical
		       	 :horizontal))
      cart))))

(defun read/cart->ppu (cart addr) 
  (let ((new-addr (map-read (cartridge-mapper cart) addr :ppu)))
    (unless (null new-addr)
      (aref (cartridge-v-char cart) new-addr))))

(defun read/cart->cpu (cart addr) 
  (let ((new-addr (map-read (cartridge-mapper cart) addr :cpu)))
    (unless (null new-addr)
      (aref (cartridge-v-prog cart) new-addr))))
      
(defun write/cart->ppu (cart addr data) 
  (let ((new-addr (map-write (cartridge-mapper cart) addr :ppu)))
    (unless (null new-addr)
      (setf (aref (cartridge-v-char cart) new-addr) data))))

(defun write/cart->cpu (cart addr data)
  (let ((new-addr (map-write (cartridge-mapper cart) addr :cpu)))
    (unless (null new-addr)
      (setf (aref (cartridge-v-prog cart) new-addr) data))))

(defmethod reset-component ((obj cartridge))
  (let ((map (cartridge-mapper obj)))
    (unless (null map)
      (reset-component map))))
