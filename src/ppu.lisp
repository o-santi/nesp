(define-register status-reg
    ((unused 5)
     (sprite-overflow 1)
     (sprite-zero-hit 1)
     (vertical-blank 1)))

(define-register mask-reg
    ((grayscale 1)
     (render-background-left 1)
     (render-sprites-left 1)
     (render-background 1)
     (render-sprites 1)
     (enhance-red 1)
     (enhance-green 1)
     (enhance-blue 1)))

(define-register control-reg
    ((nametable-x 1)
     (nametable-y 1)
     (increment-mode 1)
     (pattern-sprite 1)
     (pattern-background 1)
     (sprite-size 1)
     (slave-mode 1)
     (enable-nmi 1)))

(define-register loopy-reg
    ((coarse-x 5)
     (coarse-y 5)
     (loopy-nametable-x 1)
     (loopy-nametable-y 1)
     (fine-y 3)
     (unused 1)))

(defstruct ppu
  cartridge
  (nmi nil)
  (address-latch 0 :type u8)
  (data-buffer 0 :type u8)
  (fine-x 0 :type u8)
  (cycle 0 :type u16)
  (scanline 0 :type u16)
  (bg-next-tile-id 0)
  (bg-next-tile-attrib 0)
  (bg-next-tile-lsb 0)
  (bg-next-tile-msb 0)
  (name-table (bytememory '(2 1024)))
  (pattern-table (bytememory '(2 4096)))
  (palete-table (bytememory 32))
  (control (make-instance 'control-reg))
  (status (make-instance 'status-reg))
  (mask (make-instance 'mask-reg))
  (vram (make-instance 'loopy-reg))
  (tram (make-instance 'loopy-reg)))


(defmethod read-memory ((obj ppu) addr (from ppu) &key rdonly)
    (let ((new-addr (logand addr #x3FFF)))
      (cond
	((between new-addr #x0000 #x1FFF)
	 (let ((array-index (ash (logand new-addr #x1000) -12))
	       (memory-addr (logand new-addr #x0FFF)))
	   (aref (ppu-pattern-table ppu) array-index memory-addr))) ;; pattern table memory
	 
	((between new-addr #x2000 #x3EFF)
	 (let ((name-table (ppu-name-table obj)))
	   (case (mirror (ppu-cartridge obj))
	     (:vertical (cond
			  ((between addr #x0000 #x03FF) (aref name-table 0 (logiand #x03FF)))
			  ((between addr #x0400 #x07FF) (aref name-table 1 (logiand #x03FF)))
			  ((between addr #x0800 #x0BFF) (aref name-table 0 (logiand #x03FF)))
			  ((between addr #x0C00 #x0FFF) (aref name-table 1 (logiand #x03FF)))))
	     (:horizontal (cond
			  ((between addr #x0000 #x03FF) (aref name-table 0 (logiand #x03FF)))
			  ((between addr #x0400 #x07FF) (aref name-table 0 (logiand #x03FF)))
			  ((between addr #x0800 #x0BFF) (aref name-table 1 (logiand #x03FF)))
			  ((between addr #x0C00 #x0FFF) (aref name-table 1 (logiand #x03FF)))))))) ;; name table memory
	 
	((between new-addr #x3F00 #x3FFF)
	 (let ((masked-addr (mod (logand new-addr #x001F) #x10)))
	   (aref (ppu-palette-table ppu) masked-addr))))))

(defmethod write-memory ((obj ppu) addr data (at ppu) &key rdonly)
    (let ((new-addr (logand addr #x3FFF)))
      (cond

	((between new-addr #x0000 #x1FFF)
	 (let ((array-index (ash (logand new-addr #x1000) -12))
	       (memory-addr (logand new-addr #x0FFF)))
	   (setf (aref (ppu-pattern-table ppu) array-index memory-addr) data))) ;; pattern table memory

       	((between new-addr #x2000 #x3EFF)

	 (with-slots ((name-table (ppu-name-table obj)))
	   (case (mirror (ppu-cartridge obj))
	     (:vertical (cond
			  ((between addr #x0000 #x03FF) (setf (aref name-table 0 (logiand #x03FF)) data))
			  ((between addr #x0400 #x07FF) (setf (aref name-table 1 (logiand #x03FF)) data))
			  ((between addr #x0800 #x0BFF) (setf (aref name-table 0 (logiand #x03FF)) data))
			  ((between addr #x0C00 #x0FFF) (setf (aref name-table 1 (logiand #x03FF)) data))))
	     (:horizontal (cond
			  ((between addr #x0000 #x03FF) (setf (aref name-table 0 (logiand #x03FF)) data))
			  ((between addr #x0400 #x07FF) (setf (aref name-table 0 (logiand #x03FF)) data))
			  ((between addr #x0800 #x0BFF) (setf (aref name-table 1 (logiand #x03FF)) data))
			  ((between addr #x0C00 #x0FFF) (setf (aref name-table 1 (logiand #x03FF)) data))))))) ;; name table memory
	;; of course this could be better and more smartly done
	;; but i like this wall of parentheses at the end
	;; so idk, maybe i'll keep it
	
	((between new-addr #x3F00 #x3FFF)
	 (let ((masked-addr (mod (logand addr #x001F) #x10)))
	   (setf (aref (ppu-palette-table ppu) masked-addr) data) ;; palette memory
	 )))))

(defmethod write-memory ((obj ppu) addr data (at cl-6502:cpu))
  (case addr
    ((#x00) (progn
	      (byte->reg (ppu-control obj) data)
	      (setf (loopy-nametable-x (ppu-tram obj)) (nametable-x (ppu-control obj))) ;; control
	      (setf (loopy-nametable-y (ppu-tram obj)) (nametable-y (ppu-control obj))))) ;; control
    ((#x01) (byte->reg (ppu-mask obj) data)) ;; mask
    ((#x02) t) ;; status
    ((#x03) t) ;; OAM address
    ((#x04) t) ;; OAM data
    ((#x05) (with-slots (fine-x tram address-latch) obj
	      (cond
		((eql address-latch 0)
		 (progn
		   (setf fine-x (logand data #x07))
		   (setf (coarse-x tram) (ash data -3))
		   (setf address-latch 1)))
		((eql address-latch 1)
		 (progn
		   (setf (fine-y tram) (logand data #x07))
		   (setf (coarse-y tram) (ash data -3))
		   (setf address-latch 0)))
		))) ;; scroll

    ((#x06) (with-slots (vram tram address-latch) obj
	       (cond
		 ((eql address-latch 0) ;; ppu address
		 (progn
		   (byte->reg tram (logior (logiand (reg->byte tram) #x00FF) (ash data 8)))
		   (setf address-latch 1)))
		((eql address-latch 1)
		 (progn
		   (byte->reg tram (logior (logiand (reg->byte tram) #xFF00) data))
		   (byte->reg vram (reg->byte tram))
		   (setf address-latch 0))))))
     ((#x07) t) ;; ppu data
     ))

(defmethod read-memory ((obj ppu) addr (from cl-6502:cpu))
  (case addr
    (#x00 t) ;; control
    (#x01) t) ;; mask
    (#x02) (let ((return-data (logior
				(logiand (reg->byte (ppu-status-reg obj)) #xE0)
				(logiand (ppu-data-buffer obj) #1F))))
	      (setf (ppu-vertical-blank obj) 0)
	      (setf (ppu-address-latch obj) 0)
	      return-data)) ;; status
    (#x03 t) ;; OAM address
    (#x04 t) ;; OAM data
    (#x05 t) ;; scroll
    (#x06 t) ;; ppu address
    (#x07 ((with-slots (vram data-buffer control) obj
	       (let ((return-data data-buffer))
		     (setf data-buffer (read-memory obj (reg->byte vram) obj))
		     (when (> (reg->byte vram) #x3F00) (setf return-data data-buffer))
		     (byte->reg vram
				(+ (reg->byte vram)
				   (if (eql (increment-mode control) 1) 32 1)))
		     return-data))))
      
(defmethod clock ((obj ppu))
  (with-slots (control-reg scanline clock status-reg vertical-blank nmi vram
	       (nt-id bg-next-tile-id) (nt-attrb bg-next-tile-attrib)
	       (nt-lsb bg-next-tile-lsb) (nt-msb bg-next-tile-msb)) obj
    (when (between scanline -1 239)
      (progn
	(when (and (eql scanline -1)
		   (eql clock 1))
	  (setf (vertical-blank status-reg) 0))
	(when (or (between cycle 2 257)
		  (between cycle 321 337))
	  (case (mod (1- cycle) 8)
	    (0 (setf nt-id (read-memory obj (logior #x2000 (logiand (reg->byte vram) #x0FFF) obj))))
	    (2 (progn
		 (setf nt-attrb (read-memory obj (logior
						  (ash (loopy-nametable-y vram) 11)
						  (ash (loopy-nametable-x vram) 10)
						  (ash (ash (coarse-y vram) -2) 3)
						  (ash (coarse-x vram) -2)) obj))
		 (when-bit-set ((coarse-y vram) 1) (setf>> nt-attrb -4))
		 (when-bit-set ((coarse-x vram) 1) (setf>> nt-attrb -2))
		 (setf&& nt-attrb #x03)))
	    (4 (setf nt-lsb (read-memory obj (+
					      (ash (pattern-background control-reg) 12)
					      (ash nt-id 4)
					      (fine-y vram))
					 obj)))
	    (6 (setf nt-msb (read-memory obj (+
					      (ash (pattern-background control-reg) 12)
					      (ash nt-id 4)
					      (+ (fine-y vram) 8))
					 obj)))
	    (7 t))

  
  (when (and (eql scanline 241)
	     (eql cycle 1))
    (setf (vertical-blank status-reg) 1)
    (when (eq (enable-nmi control-reg 1))
      (setf nmi t)))



    ))
