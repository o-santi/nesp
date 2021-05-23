(in-package #:nes)

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

(define-register sprite-entry
  ((y 8)
   (id 8)
   (attribute 8)
   (x 8)))

(defstruct ppu
  cartridge
  frame-complete
  (nmi-bit nil)
  (address-latch 0 :type u8)
  (data-buffer 0 :type u8)
  (fine-x 0 :type u8)
  (oam-addr 0 :type u8)
  (cycle 0 :type u16)
  (scanline 0 :type (signed-byte 16))
  (bg-shifter-pattern-lo 0 :type u16)
  (bg-shifter-pattern-hi 0 :type u16)
  (bg-shifter-attrib-lo  0 :type u16)
  (bg-shifter-attrib-hi 0 :type u16)
  (bg-next-tile-id 0 :type u8)
  (bg-next-tile-attrib 0 :type u8)
  (bg-next-tile-lsb 0 :type u8)
  (bg-next-tile-msb 0 :type u8)
  (name-table (bytememory '(2 1024)))
  (pattern-table (bytememory '(2 4096)))
  (palette-table (bytememory 32))
  (control (make-instance 'control-reg))
  (status (make-instance 'status-reg))
  (mask (make-instance 'mask-reg))
  (vram (make-instance 'loopy-reg))
  (tram (make-instance 'loopy-reg))
  (oam (make-array 64 :element-type 'sprite-entry :initial-element (make-instance 'sprite-entry))))

(defmethod reset-component ((obj ppu))
  (setf (ppu-fine-x obj) 0)
  (setf (ppu-address-latch obj) 0)
  (setf (ppu-data-buffer obj) 0)
  (setf (ppu-scanline obj) 0)
  (setf (ppu-cycle obj) 0)
  (setf (ppu-bg-shifter-pattern-lo obj) 0)
  (setf (ppu-bg-shifter-pattern-hi obj) 0)
  (setf (ppu-bg-shifter-attrib-lo obj) 0)
  (setf (ppu-bg-shifter-attrib-hi obj) 0)
  (setf (ppu-bg-next-tile-id obj) 0)
  (setf (ppu-bg-next-tile-attrib obj) 0)
  (setf (ppu-bg-next-tile-lsb obj) 0)
  (setf (ppu-bg-next-tile-msb obj) 0)
  (setf (reg (ppu-control obj)) 0)
  (setf (reg (ppu-status obj)) 0)
  (setf (reg (ppu-mask obj)) 0)
  (setf (reg (ppu-vram obj)) 0)
  (setf (reg (ppu-tram obj)) 0))

(defun insert-cartridge-in-ppu (cartridge ppu)
  (setf (ppu-cartridge ppu) cartridge))

(defun read/ppu->ppu (ppu addr)
    (let ((new-addr (logand addr #x3FFF)))
      (cond
	((read/cart->ppu (ppu-cartridge ppu) addr) (read/cart->ppu (ppu-cartridge ppu) addr))
	((between new-addr #x0000 #x1FFF)
	 (let ((array-index (ash (logand new-addr #x1000) -12))
	       (memory-addr (logand new-addr #x0FFF)))
	   (aref (ppu-pattern-table ppu) array-index memory-addr))) ;; pattern table memory
	 
	((between new-addr #x2000 #x3EFF)
	 (with-slots (name-table cartridge palette-table) ppu
	    (let ((new-new-addr (logand new-addr #x0FFF)))
	      (case (cartridge-mirror cartridge)
		(:vertical (cond
			     ((between new-new-addr #x0000 #x03FF) (aref name-table 0 (logand new-new-addr #x03FF)))
			     ((between new-new-addr #x0400 #x07FF) (aref name-table 1 (logand new-new-addr #x03FF)))
			     ((between new-new-addr #x0800 #x0BFF) (aref name-table 0 (logand new-new-addr #x03FF)))
			     ((between new-new-addr #x0C00 #x0FFF) (aref name-table 1 (logand new-new-addr #x03FF)))))
		(:horizontal (cond
			       ((between new-new-addr #x0000 #x03FF) (aref name-table 0 (logand new-new-addr #x03FF)))
			       ((between new-new-addr #x0400 #x07FF) (aref name-table 0 (logand new-new-addr #x03FF)))
			       ((between new-new-addr #x0800 #x0BFF) (aref name-table 1 (logand new-new-addr #x03FF)))
			       ((between new-new-addr #x0C00 #x0FFF) (aref name-table 1 (logand new-new-addr #x03FF))))))))) ;; name table memory
	
	((between new-addr #x3F00 #x3FFF)
	 (let ((addr (logand new-addr #x001F)))
	   (when (eql addr #x10) (setf addr #x00))
	   (when (eql addr #x14) (setf addr #x04))
	   (when (eql addr #x18) (setf addr #x08))
	   (when (eql addr #x1C) (setf addr #x0C))
	   (logand (aref (ppu-palette-table ppu) addr) (if (bit-set (grayscale (ppu-mask ppu))) #x30 #x3F)))))))

(defun write/ppu->ppu (ppu addr data)
  (let ((new-addr (logand addr #x3FFF)))
    (cond
      ((write/cart->ppu (ppu-cartridge ppu) addr data))
      ((between new-addr #x0000 #x1FFF)
       (let ((array-index (ash (logand new-addr #x1000) -12))
	     (memory-addr (logand new-addr #x0FFF)))
	 (setf (aref (ppu-pattern-table ppu) array-index memory-addr) data))) ;; pattern table memory
      
      ((between new-addr #x2000 #x3EFF)
	  (with-slots (name-table cartridge) ppu
	    (let ((new-new-addr (logand new-addr #x0FFF)))
	      (case (cartridge-mirror cartridge)
		(:vertical (cond
			     ((between new-new-addr #x0000 #x03FF) (setf (aref name-table 0 (logand new-new-addr #x03FF)) data))
			     ((between new-new-addr #x0400 #x07FF) (setf (aref name-table 1 (logand new-new-addr #x03FF)) data))
			     ((between new-new-addr #x0800 #x0BFF) (setf (aref name-table 0 (logand new-new-addr #x03FF)) data))
			     ((between new-new-addr #x0C00 #x0FFF) (setf (aref name-table 1 (logand new-new-addr #x03FF)) data))))
		(:horizontal (cond
			       ((between new-new-addr #x0000 #x03FF) (setf (aref name-table 0 (logand new-new-addr #x03FF)) data))
			       ((between new-new-addr #x0400 #x07FF) (setf (aref name-table 0 (logand new-new-addr #x03FF)) data))
			       ((between new-new-addr #x0800 #x0BFF) (setf (aref name-table 1 (logand new-new-addr #x03FF)) data))
			       ((between new-new-addr #x0C00 #x0FFF) (setf (aref name-table 1 (logand new-new-addr #x03FF)) data))))))))
	  ;; name table memory
	  ;; of course this could be better and more smartly done
	  ;; but i like this wall of parentheses at the end
	  ;; so idk, maybe i'll keep it
	  
	((between new-addr #x3F00 #x3FFF)
	 (let ((addr (logand new-addr #x001F)))
	   (when (eql addr #x10) (setf addr #x00))
	   (when (eql addr #x14) (setf addr #x04))
	   (when (eql addr #x18) (setf addr #x08))
	   (when (eql addr #x1C) (setf addr #x0C))
	   (setf (aref (ppu-palette-table ppu) addr) data))))))

(defun write/ppu->cpu (ppu addr data)
  (case addr
    (#x00 (with-slots (control tram) ppu
	       (progn
		 (setf (reg control) data)
		 (setf (loopy-nametable-x tram) (nametable-x control)) ;; control
		 (setf (loopy-nametable-y tram) (nametable-y control))))) ;; control
    (#x01 (setf (reg (ppu-mask ppu)) data)) ;; mask
    (#x02 nil) ;; status
    (#x03 nil) ;; OAM address
    (#x04 nil) ;; OAM data
    (#x05 (with-slots (fine-x tram address-latch) ppu
	      (if (not (bit-set address-latch))
		   (progn
		     (setf fine-x (logand data #x07))
		     (setf (coarse-x tram) (ash data -3))
		     (setf address-latch 1)) ;; writing just for completion, here just `t` would suffice
		   (progn
		     (setf (fine-y tram) (logand data #x07))
		     (setf (coarse-y tram) (ash data -3))
		     (setf address-latch 0))))) ;; scroll

    (#x06 (with-slots (vram tram address-latch) ppu
	       (cond
		 ((not (bit-set address-latch)) ;; ppu address
		 (progn
		   (setf (reg tram) (logior (logand (reg tram) #x00FF) (ash data 8)))
		   (setf address-latch 1)))
		((bit-set address-latch) ;; writing just for completion, `t` suffices
		 (progn
		   (setf (reg tram) (logior (logand (reg tram) #xFF00) data))
		   (setf (reg vram) (reg tram))
		   (setf address-latch 0))))))
    (#x07 (progn
	    (write/ppu->ppu ppu (reg (ppu-vram ppu)) data)
	    (incf (reg (ppu-vram ppu)) (if (bit-set (increment-mode (ppu-control ppu))) 32 1))));; ppu data
     ))

(defun read/ppu->cpu (ppu addr)
  (case addr
    (#x00 nil) ;; control
    (#x01 nil) ;; mask
    (#x02 (let ((return-data (logior
				(logand (reg (ppu-status ppu)) #xE0)
				(logand (ppu-data-buffer ppu) #x1F))))
	      (setf (vertical-blank (ppu-status ppu)) 0)
	      (setf (ppu-address-latch ppu) 0)
	      return-data)) ;; status
    (#x03 nil) ;; OAM address
    (#x04 0) ;; OAM data
    (#x05 nil) ;; scroll
    (#x06 nil) ;; ppu address
    (#x07 (with-slots (vram data-buffer control) ppu
	    (let ((return-data data-buffer))
	      (setf data-buffer (read/ppu->ppu ppu (reg vram)))
	      (when (> (reg vram) #x3F00) (setf return-data data-buffer))
	      (setf (reg vram)
		    (+ (reg vram)
		       (if (bit-set (increment-mode control)) 32 1)))
	      return-data)))))

(defun increment-scroll-x (ppu)
  (with-slots (mask vram) ppu
    (when (or (bit-set (render-background mask))
	      (bit-set (render-sprites mask)))
      (if (eql (coarse-x vram) 31)
	  (progn
	    (setf (coarse-x vram) 0)
	    (setf (loopy-nametable-x vram) (logxor 1 (loopy-nametable-x vram))))
	  (incf (coarse-x vram))))))

(defun increment-scroll-y (ppu)
  (with-slots (mask vram) ppu
    (when (or (bit-set (render-background mask))
	      (bit-set (render-sprites mask)))
      (with-register-bits (fine-y coarse-y loopy-nametable-x loopy-nametable-y) vram
       (if (< fine-y 7)
	  (incf fine-y)
	  (progn
	    (setf fine-y 0)
	    (case coarse-y
	      (29
	       (progn (setf coarse-y 0)
		      (setf loopy-nametable-y (logxor loopy-nametable-y 1))))
	      (31 (setf coarse-y 0))
	      (otherwise (incf coarse-y)))))))))

(defun transfer-address-x (ppu)
  (with-slots (mask vram tram) ppu
    (when (or (bit-set (render-background mask))
	      (bit-set (render-sprites mask)))
      (setf (loopy-nametable-x vram) (loopy-nametable-x tram))
      (setf (coarse-x vram) (coarse-x tram)))))

(defun transfer-address-y (ppu)
  (with-slots (mask vram tram) ppu
    (when (or (bit-set (render-background mask))
	      (bit-set (render-sprites mask)))
      (setf (fine-y vram) (fine-y tram))
      (setf (loopy-nametable-y vram) (loopy-nametable-y tram))
      (setf (coarse-y vram) (coarse-y tram)))))

(defun load-background-shifters (ppu)
  (with-slots ((pattern-lo bg-shifter-pattern-lo) (pattern-hi bg-shifter-pattern-hi)
	       (attrib-lo bg-shifter-attrib-lo) (attrib-hi bg-shifter-attrib-hi)
	       (nt-lsb bg-next-tile-lsb) (nt-msb bg-next-tile-msb) (nt-attrb bg-next-tile-attrib)) ppu
    (setf pattern-lo (logior (logand pattern-lo #xFF00) nt-lsb))
    (setf pattern-hi (logior (logand pattern-hi #xFF00) nt-msb))
    (setf attrib-lo (logior (logand attrib-lo #xFF00) (if (logbitp 0 nt-attrb) #xFF #x00)))
    (setf attrib-hi (logior (logand attrib-hi #xFF00) (if (logbitp 1 nt-attrb) #xFF #x00)))))

(defun update-shifters (ppu)
  (with-slots ((pattern-lo bg-shifter-pattern-lo) (pattern-hi bg-shifter-pattern-hi)
	       (attrib-lo bg-shifter-attrib-lo) (attrib-hi bg-shifter-attrib-hi)
	       mask) ppu
    (when (bit-set (render-background mask))
      (setf pattern-lo (logand (ash pattern-lo 1) #xFFFF))
      (setf pattern-hi (logand (ash pattern-hi 1) #xFFFF))
      (setf attrib-lo (logand (ash attrib-lo 1) #xFFFF))
      (setf attrib-hi (logand (ash attrib-hi 1) #xFFFF)))))
      
(defun set-pixel (rgb x y)
  (destructuring-bind (red green blue) rgb
    (let ((x (* 4 x))
	  (y (* 4 y)))
      (let ((rend (lambda (renderer) (sdl2:set-render-draw-color renderer red green blue #xFF)))
	    (draw (lambda (renderer) (progn
				       (sdl2:render-draw-point renderer x y)
				       (sdl2:render-draw-point renderer (1+ x) y)
				       (sdl2:render-draw-point renderer x (1+ y))
				       (sdl2:render-draw-point renderer (1+ x) (1+ y))))))
	(vector-push-extend (list rend draw) *pixels*)))))
  
(defun clock-ppu (ppu)
  (with-slots (control scanline status vertical-blank
	       nmi-bit vram fine-x frame-complete cycle mask
	       (nt-id bg-next-tile-id) (nt-attrb bg-next-tile-attrib)
	       (pattern-lo bg-shifter-pattern-lo) (pattern-hi bg-shifter-pattern-hi)
	       (attrib-lo bg-shifter-attrib-lo) (attrib-hi bg-shifter-attrib-hi)
	       (nt-lsb bg-next-tile-lsb) (nt-msb bg-next-tile-msb)) ppu
    
    (when (between scanline -1 239)
      
      (when (and (eql scanline 0)
		 (eql cycle 0))
	(setf cycle 1))

      
      (when (and (eql scanline -1)
		 (eql cycle 1))
	(setf (vertical-blank status) 0))

	
      (when (or (between cycle 2 257)
		(between cycle 321 337))
	(update-shifters ppu)
	(case (mod (1- cycle) 8)
	  (0 (progn
	       (load-background-shifters ppu)
	       (setf nt-id (read/ppu->ppu ppu (logior #x2000 (logand (reg vram) #x0FFF))))))
	  (2 (progn
	       (setf nt-attrb (read/ppu->ppu ppu (logior
						  #x23C0
						  (ash (loopy-nametable-y vram) 11)
						  (ash (loopy-nametable-x vram) 10)
						  (ash (ash (coarse-y vram) -2) 3)
						  (ash (coarse-x vram) -2))))
	       (when (bit-set (coarse-y vram) :pos 1) (setf>> nt-attrb -4))
	       (when (bit-set (coarse-x vram) :pos 1) (setf>> nt-attrb -2))
	       (setf&& nt-attrb #x03)))
	  (4 (setf nt-lsb (read/ppu->ppu ppu (+
					      (ash (pattern-background control) 12)
					      (ash nt-id 4)
					      (fine-y vram)))))
	  (6 (setf nt-msb (read/ppu->ppu ppu (+
					      (ash (pattern-background control) 12)
					      (ash nt-id 4)
					      (+ (fine-y vram) 8)))))
	  (7 (increment-scroll-x ppu))))

	
      (when (eql cycle 256)
	(increment-scroll-y ppu))

	
      (when (eql cycle 257)
	(load-background-shifters ppu)
	(transfer-address-x ppu))

	
      (when (or (eql cycle 338)
		(eql cycle 340))
	(setf nt-id (read/ppu->ppu ppu (logior #x2000 (logand (reg vram) #x0FFF)))))
      
      (when (and (eql scanline -1)
		 (>= cycle 280)
		 (< cycle 305))
	(transfer-address-y ppu)))
    ;;
    ;; end of visible cycle
    ;;
    (when (between scanline 241 260)
      (when (and (eql scanline 241)
		 (eql cycle 1))
	(setf (vertical-blank status) 1)
	(when (bit-set (enable-nmi control))
	  (setf nmi-bit t))))
    
    (let ((bg-pixel 0)
	  (bg-palette 0))
      
      (when (bit-set (render-background mask)) 
	(let* ((bit-mux (ash #x8000 (- fine-x)))
	       (p0-pixel (if (> (logand pattern-lo bit-mux) 0) 1 0))
	       (p1-pixel (if (> (logand pattern-hi bit-mux) 0) 1 0))
	       (bg-pal0 (if (> (logand attrib-lo bit-mux) 0) 1 0))
	       (bg-pal1 (if (> (logand attrib-hi bit-mux) 0) 1 0)))
	  (setf bg-pixel (logior (ash p1-pixel 1) p0-pixel))
	  (setf bg-palette (logior (ash bg-pal1 1) bg-pal0))))
      (set-pixel (get-color-from-palette-ram ppu bg-palette bg-pixel) (1- cycle) scanline))
    (incf cycle)
    
    (when (>= cycle 341)
      (setf cycle 0)
      (incf scanline)
      (when (>= scanline 261)
	(setf scanline -1)
	(setf frame-complete t)))))
