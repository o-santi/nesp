(defstruct cartridge
  filename
  mapper 
  mirror
  (prog-banks 0 :type u8)
  (char-banks 0 :type u8) 
  (v-prog-mem (make-array 0 :fill-pointer 0 :adjustable t))
  (v-char-mem (make-array 0 :fill-pointer 0 :adjustable t)))

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

(defun create-mapper (id)
  (let ((mapper-string (make-symbol (concatenate 'string "mapper-" (format nil "~3,'0D" id)))))
    (make-instance mapper-string)))

(defun parse-cartridge (filename)
  (with-open-file (filestream filename :element-type 'u8)
    (let* ((header (read-value 'cartridge-header filestream))
					;(mapper-id (get-mapper-id header))
	   (cartridge (make-cartridge))
	   (prg-mem (* 16384 (bit-vector->integer (pgr-rom-size header))))
	   (chr-mem (* 8192 (bit-vector->integer (chr-rom-size header)))))

      (setf (cartridge-mapper cartridge) (create-mapper (get-mapper-id (header))))
      
      (read-value 'u8 filestream :length 512) ;; just junk, does not matter
      ;; adjust the size of the virtual program memory to the cartridge struct
      (adjust-array (cartridge-v-prog-mem cartridge) prg-mem)
      ;;read into it
      (setf (cartridge-v-prog-mem cartridge) (read-value 'u8 filestream :length prg-mem))
      ;; adjust the size of the character program memory
      (adjust-array (cartridge-v-prog-mem cartridge) chr-mem)
      ;; read into it
      (setf (cartridge-v-prog-mem cartridge) (read-value 'u8 filestream :length chr-mem))
      (setf (cartridge-mirror cartridge) (if (eql (logiand (mapper1 header) #x01) 1)
					     :vertical
					     :horizontal))
      
    cartridge)))

(defmethod read-memory ((obj cartridge) addr &key at)
  (let ((new-addr (map-read (cartridge-mapper obj) addr)))
    (when new-addr
      (cond
	((cl-6502:cpu-p at)
	 (aref (cartridge-v-prog-mem cartridge) new-addr))
	((ppu-p at)
	 (aref (cartridge-c-char-mem cartridge) new-addr))))))
  
      
(defmethod write-memory ((obj cartridge) addr data &key at)
  (let ((new-addr (map-write (cartridge-mapper obj) addr)))
    (when new-addr
      (cond
	((cl-6502:cpu-p at)
	 (setf (aref (cartridge-v-prog-mem cartridge) new-addr) data))
	((ppu-p at)
	 (setf (aref (cartridge-v-char-mem cartridge) new-addr) data))))))
