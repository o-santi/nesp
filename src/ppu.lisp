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
     (sprite-id 8)
     (attribute 8)
     (x 8)))

(defstruct ppu
  renderer
  last-drawn-color
  cartridge
  (frame-complete nil :type boolean)
  (nmi-bit nil :type boolean)
  (sprite-zero-hit-possible nil :type boolean)
  (sprite-zero-hit-being-rendered nil :type boolean)
  (address-latch 0 :type bit)
  (data-buffer 0 :type u8)
  (fine-x 0 :type u8)
  (oam-addr 0 :type u8)
  (sprite-count 0 :type u8)
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
  (name-table (bytememory '(2 1024)) :type (simple-array u8 (2 1024)))
  (pattern-table (bytememory '(2 4096)) :type (simple-array u8 (2 4096)))
  (palette-table (bytememory 32) :type (simple-array u8 (32)))
  (control (make-instance 'control-reg) :type control-reg)
  (status (make-instance 'status-reg) :type status-reg)
  (mask (make-instance 'mask-reg) :type mask-reg)
  (vram (make-instance 'loopy-reg) :type loopy-reg)
  (tram (make-instance 'loopy-reg) :type loopy-reg)
  (oam (make-array 64))
  (sprite-scanline (make-array 8))
  (sprite-shifter-pattern-lo (bytememory 8))
  (sprite-shifter-pattern-hi (bytememory 8)))

(defmethod reset-component ((obj ppu))
  (setf (ppu-frame-complete obj) nil)
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
  (setf (reg (ppu-tram obj)) 0)
  (map-into (ppu-oam obj) (lambda () (make-instance 'sprite-entry)))
  (map-into (ppu-sprite-scanline obj) (lambda () (make-instance 'sprite-entry))))

(defun insert-cartridge-in-ppu (cartridge ppu)
  (setf (ppu-cartridge ppu) cartridge))

(declaim (ftype (function (ppu u16) u8) read/ppu->ppu))
(defun read/ppu->ppu (ppu addr)
  (declare (optimize (speed 3) (safety 0))
	   (values u8))
  (with-slots (cartridge pattern-table name-table palette-table mask) ppu
    (declare (type (simple-array u8) pattern-table name-table palette-table))
    (let ((new-addr (logand addr #x3FFF)))
      (cond
	((read/cart->ppu cartridge new-addr) (read/cart->ppu cartridge new-addr))
	((between new-addr #x0000 #x1FFF)
	 (let ((array-index (ldb (byte 1 12) new-addr))
	       (memory-addr (logand new-addr #x0FFF)))
	   (aref pattern-table array-index memory-addr))) ;; pattern table memory
	
	((between new-addr #x2000 #x3EFF)
	 (let* ((addr (logand new-addr #x0FFF))
		(final-addr (logand addr #x03FF))) 
	   (case (cartridge-mirror cartridge)
	     (:vertical (cond
			  ((between addr #x0000 #x03FF) (aref name-table 0 final-addr))
			  ((between addr #x0400 #x07FF) (aref name-table 1 final-addr))
			  ((between addr #x0800 #x0BFF) (aref name-table 0 final-addr))
			  ((between addr #x0C00 #x0FFF) (aref name-table 1 final-addr))))
	     (:horizontal (cond
			    ((between addr #x0000 #x03FF) (aref name-table 0 final-addr))
			    ((between addr #x0400 #x07FF) (aref name-table 0 final-addr))
			    ((between addr #x0800 #x0BFF) (aref name-table 1 final-addr))
			    ((between addr #x0C00 #x0FFF) (aref name-table 1 final-addr))))))) ;; name table memory
	;; there may be a better way to do this
	;; but i like this parens wall
	((between new-addr #x3F00 #x3FFF)
	 (let ((addr (logand new-addr #x001F)))
	   (when (eql addr #x10) (setf addr #x00))
	   (when (eql addr #x14) (setf addr #x04))
	   (when (eql addr #x18) (setf addr #x08))
	   (when (eql addr #x1C) (setf addr #x0C))
	   (logand (aref palette-table addr) (if (bit-set (grayscale mask)) #x30 #x3F))))
	(t 0)))))

(declaim (ftype (function (ppu u16 u8)) write/ppu->ppu)) 
(defun write/ppu->ppu (ppu addr data)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (cartridge pattern-table name-table palette-table) ppu
    (declare (type (simple-array u8) pattern-table name-table palette-table))
    (let ((new-addr (logand addr #x3FFF)))
      (cond
	((write/cart->ppu cartridge new-addr data))
	((between new-addr #x0000 #x1FFF)
	 (let ((array-index (ldb (byte 1 12) new-addr))
	       (memory-addr (logand new-addr #x0FFF)))
	   (setf (aref pattern-table array-index memory-addr) data))) ;; pattern table memory
	
	((between new-addr #x2000 #x3EFF)
	 (let* ((addr (logand new-addr #x0FFF))
		(final-addr (logand addr #x03FF)))
	   (case (cartridge-mirror cartridge)
	     (:vertical (cond
			  ((between addr #x0000 #x03FF) (setf (aref name-table 0 final-addr) data))
			  ((between addr #x0400 #x07FF) (setf (aref name-table 1 final-addr) data))
			  ((between addr #x0800 #x0BFF) (setf (aref name-table 0 final-addr) data))
			  ((between addr #x0C00 #x0FFF) (setf (aref name-table 1 final-addr) data))))
	     (:horizontal (cond
			    ((between addr #x0000 #x03FF) (setf (aref name-table 0 final-addr) data))
			    ((between addr #x0400 #x07FF) (setf (aref name-table 0 final-addr) data))
			    ((between addr #x0800 #x0BFF) (setf (aref name-table 1 final-addr) data))
			    ((between addr #x0C00 #x0FFF) (setf (aref name-table 1 final-addr) data)))))))
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
	   (setf (aref palette-table addr) data)))))))

(declaim (ftype (function (ppu u16)) read-oam))
(defun read-oam (ppu addr)
  (with-slots (oam) ppu
    (declare (optimize (speed 3) (safety 0))
	     (type (simple-array sprite-entry) oam)
	     (values u8))
    (let* ((sprite-obj (aref oam (ldb (byte 6 2) addr))) ;; OAM data
	   (attr-id (ldb (byte 2 0) addr)))
      (case attr-id
	(0 (y sprite-obj))
	(1 (sprite-id sprite-obj))
	(2 (attribute sprite-obj))
	(3 (x sprite-obj))))))

(declaim (ftype (function (ppu u16 u8)) write-oam))
(defun write-oam (ppu addr data)
  (with-slots (oam) ppu
    (declare (optimize (speed 3) (safety 0))
	     (type (simple-array sprite-entry) oam)
	     (values u8))
    (let* ((sprite-obj (aref oam (ldb (byte 6 2) addr))) ;; OAM data
	   (attr-id (ldb (byte 2 0) addr)))
      (case attr-id
	(0 (setf (y sprite-obj) data))
	(1 (setf (sprite-id sprite-obj) data))
	(2 (setf (attribute sprite-obj) data))
	(3 (setf (x sprite-obj) data))))))

(declaim (ftype (function (ppu u16 u8)) write/ppu->cpu))
(defun write/ppu->cpu (ppu addr data)
  (with-slots (control vram tram fine-x address-latch mask oam-addr) ppu
    (declare (optimize (speed 3) (safety 0)))
    (case addr
      (#x00 (progn
	      (setf (reg control) data)
	      (setf (loopy-nametable-x tram) (nametable-x control)) ;; control
	      (setf (loopy-nametable-y tram) (nametable-y control)))) ;; control
      (#x01 (setf (reg mask) data)) ;; mask
      (#x02 nil) ;; status
      (#x03 (setf oam-addr data)) ;; OAM address
      (#x04 (write-oam ppu addr data)) ;; OAM data
      (#x05 (if (not (bit-set address-latch))
		(progn
		  (setf fine-x (logand data #x07))
		  (setf (coarse-x tram) (ash data -3))
		  (setf address-latch 1)) 
		(progn
		  (setf (fine-y tram) (logand data #x07))
		  (setf (coarse-y tram) (ash data -3))
		  (setf address-latch 0)))) ;; scroll

      (#x06 (if (not (bit-set address-latch)) ;; ppu address
		(progn
		  (setf (reg tram) (logior (logand (reg tram) #x00FF) (ash (logand data #x3F) 8)))
		  (setf address-latch 1))
		(progn
		  (setf (reg tram) (logior (logand (reg tram) #xFF00) data))
		  (setf (reg vram) (reg tram))
		  (setf address-latch 0))))
      (#x07 (progn
	      (write/ppu->ppu ppu (reg vram) data)
	      (incf (reg vram) (if (bit-set (increment-mode control)) 32 1)))))))


(declaim (ftype (function (ppu u16) u8) read/ppu->cpu))
(defun read/ppu->cpu (ppu addr)
  (declare (optimize (speed 3) (safety 0))
	   (values u8))
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
    (#x04 (read-oam ppu addr)) ;; OAM data
	    
    (#x05 nil) ;; scroll
    (#x06 nil) ;; ppu address
    (#x07 (with-slots (vram data-buffer control) ppu
	    (let ((return-data data-buffer))
	      (setf data-buffer (read/ppu->ppu ppu (reg vram)))
	      (when (> (reg vram) #x3F00) (setf return-data data-buffer))
	      (incf (reg vram) (if (bit-set (increment-mode control)) 32 1))
	      return-data)))))

(declaim (ftype (function (ppu)) increment-scroll-x))
(defun increment-scroll-x (ppu)
  (with-slots (mask vram) ppu
    (with-register-bits (coarse-x loopy-nametable-x) vram
      (declare (optimize (speed 3) (safety 0))
	       (type fixnum coarse-x))
      (when (or (bit-set (render-background mask))
		(bit-set (render-sprites mask)))
	(if (eql (coarse-x vram) 31)
	    (progn
	      (setf coarse-x 0)
	      (setf loopy-nametable-x (invert-bit loopy-nametable-x)))
	    (incf coarse-x))))))

(declaim (ftype (function (ppu)) increment-scroll-y))
(defun increment-scroll-y (ppu)
  (with-slots (mask vram) ppu
    (declare (optimize (speed 3) (safety 0)))
    (when (or (bit-set (render-background mask))
	      (bit-set (render-sprites mask)))
      (with-register-bits (fine-y coarse-y loopy-nametable-x loopy-nametable-y) vram
	(declare (type fixnum fine-y coarse-y))
	(if (< fine-y 7)
	    (incf fine-y)
	    (progn
	      (setf fine-y 0)
	      (case coarse-y
		(29
		 (progn (setf coarse-y 0)
			(setf loopy-nametable-y (invert-bit loopy-nametable-y))))
		(31 (setf coarse-y 0))
		(otherwise (incf coarse-y)))))))))

(declaim (ftype (function (ppu)) transfer-address-x))
(defun transfer-address-x (ppu)
  (with-slots (mask vram tram) ppu
    (declare (optimize (speed 3) (safety 0)))
    (when (or (bit-set (render-background mask))
	      (bit-set (render-sprites mask)))
      (setf (loopy-nametable-x vram) (loopy-nametable-x tram))
      (setf (coarse-x vram) (coarse-x tram)))))

(declaim (ftype (function (ppu)) transfer-address-y))
(defun transfer-address-y (ppu)
  (with-slots (mask vram tram) ppu
    (declare (optimize (speed 3) (safety 0)))
    (when (or (bit-set (render-background mask))
	      (bit-set (render-sprites mask)))
      (setf (fine-y vram) (fine-y tram))
      (setf (loopy-nametable-y vram) (loopy-nametable-y tram))
      (setf (coarse-y vram) (coarse-y tram)))))

(defun load-background-shifters (ppu)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots ((pattern-lo bg-shifter-pattern-lo) (pattern-hi bg-shifter-pattern-hi)
	       (attrib-lo bg-shifter-attrib-lo) (attrib-hi bg-shifter-attrib-hi)
	       (nt-lsb bg-next-tile-lsb) (nt-msb bg-next-tile-msb) (nt-attrb bg-next-tile-attrib)) ppu
    (declare (type fixnum pattern-lo pattern-hi nt-lsb nt-msb))
    (setf pattern-lo (logior (logand pattern-lo #xFF00) nt-lsb))
    (setf pattern-hi (logior (logand pattern-hi #xFF00) nt-msb))
    (setf attrib-lo (logior (logand attrib-lo #xFF00) (if (logbitp 0 nt-attrb) #xFF #x00)))
    (setf attrib-hi (logior (logand attrib-hi #xFF00) (if (logbitp 1 nt-attrb) #xFF #x00)))))

(defun update-shifters (ppu)
  (with-slots ((pattern-lo bg-shifter-pattern-lo) (pattern-hi bg-shifter-pattern-hi)
	       (attrib-lo bg-shifter-attrib-lo) (attrib-hi bg-shifter-attrib-hi)
	       (s-patt-hi sprite-shifter-pattern-hi) (s-patt-lo sprite-shifter-pattern-lo)
	       mask cycle sprite-count sprite-scanline) ppu
    (declare (optimize (speed 3) (safety 0))
	     (type fixnum cycle)
	     (type (simple-array u8) s-patt-hi s-patt-lo)
	     (type (simple-array sprite-entry) sprite-scanline))
    (when (bit-set (render-background mask))
      (setf pattern-lo (logand (ash pattern-lo 1) #xFFFF))
      (setf pattern-hi (logand (ash pattern-hi 1) #xFFFF))
      (setf attrib-lo (logand (ash attrib-lo 1) #xFFFF))
      (setf attrib-hi (logand (ash attrib-hi 1) #xFFFF)))
    (when (and (bit-set (render-sprites mask))
	       (between cycle 1 257))
      (loop-for-element-in sprite-scanline
	(with-register-bits (x) element
	  (declare (type fixnum x))
	  (if (> x 0)
	      (decf x)
	      (progn
		(setf (aref s-patt-hi i) (logand #xFF (ash (aref s-patt-hi i) 1)))
		(setf (aref s-patt-lo i) (logand #xFF (ash (aref s-patt-lo i) 1))))))))))
      
(defun set-pixel (ppu rgb x y)
  (declare (optimize (speed 3) (safety 0)))
  (destructuring-bind (red green blue) rgb
    (with-slots (renderer last-drawn-color) ppu
      (when (not (equal rgb last-drawn-color))
	(sdl2:set-render-draw-color renderer red green blue #xFF)
	(setf last-drawn-color rgb))
      (sdl2:render-draw-point renderer x y))))

(declaim (ftype (function (ppu)) clock-cpu))
(defun clock-ppu (ppu)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (control scanline status vertical-blank
	       sprite-zero-hit-possible sprite-zero-hit-being-rendered
	       nmi-bit vram fine-x frame-complete cycle mask
	       sprite-scanline oam oam-addr sprite-count
	       sprite-shifter-pattern-hi sprite-shifter-pattern-lo
	       (nt-id bg-next-tile-id) (nt-attrb bg-next-tile-attrib)
	       (pattern-lo bg-shifter-pattern-lo) (pattern-hi bg-shifter-pattern-hi)
	       (attrib-lo bg-shifter-attrib-lo) (attrib-hi bg-shifter-attrib-hi)
	       (nt-lsb bg-next-tile-lsb) (nt-msb bg-next-tile-msb)) ppu
    (declare (type (simple-array u8) sprite-shifter-pattern-hi sprite-shifter-pattern-lo)
	     (type (simple-array sprite-entry) oam sprite-scanline)
	     (type fixnum scanline cycle sprite-count)
	     (type u8 nt-id fine-x))
    (when (between scanline -1 239)
      (when (and (eql scanline 0)
		 (eql cycle 0))
	(setf cycle 1))

      
      (when (and (eql scanline -1)
		 (eql cycle 1))
	(setf (vertical-blank status) 0)
	(setf (sprite-overflow status) 0)
	(setf (sprite-zero-hit status) 0)
	(loop-for-element-in sprite-shifter-pattern-hi
	  (setf element 0))
	(loop-for-element-in sprite-shifter-pattern-lo
	  (setf element 0)))
      
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
	       (setf nt-attrb (logand nt-attrb #x03))))
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
	(transfer-address-y ppu))
      
      ;; =======================================
      ;; end of visible scanline
      ;; start of foreground rendering
      ;; the nes does not do it this way but its easier to do
      ;; so who cares
      ;; =======================================
      
      (when (and (eql cycle 257)
		 (>= scanline 0))
	(loop-for-element-in sprite-scanline
	  (setf (reg element) #xFFFFFFFF))
	(loop-for-element-in sprite-shifter-pattern-hi
	  (setf element 0))
	(loop-for-element-in sprite-shifter-pattern-lo
	  (setf element 0))
	(setf sprite-count 0)
	(setf sprite-zero-hit-possible nil)
	(let ((n-oam-entry 0))
	  (loop while (and (< n-oam-entry 64)
			   (< sprite-count 9))
		    do (let ((diff (- scanline (y (aref oam n-oam-entry)))))
		     (when (and (>= diff 0)
				(< diff (if (bit-set (sprite-size control)) 16 8)))
		       (when (< sprite-count 8)
			 (when (eql n-oam-entry 0)
			   (setf sprite-zero-hit-possible t))
			 (setf (reg (aref sprite-scanline sprite-count)) (reg (aref oam n-oam-entry)))
			 (incf sprite-count)))
		       (incf n-oam-entry))))
		   (setf (sprite-overflow status) (if (> sprite-count 8) 1 0)))

      (when (eql cycle 340)
	(loop-for-element-in sprite-scanline
	  (let ((bits-lo 0)
		(bits-hi 0)
		(addr-lo 0)
		(addr-hi 0))
	    (declare (type u8 bits-lo bits-hi addr-lo addr-hi))
	    (with-register-bits (y sprite-id attribute) element
	      (if (not (bit-set (sprite-size control)))
		  ;; if sprites are 8x8
		  (if (not (logbitp 7 attribute))
		      ;; if sprite is not flipped vertically
		      (setf addr-lo (logior (ash (pattern-sprite control) 12)
					    (ash sprite-id 4)
					    (- scanline y)))
		      ;; sprite is flipped vertically
		      (setf addr-lo (logior (ash (pattern-sprite control) 12)
					    (ash sprite-id 4)
					    (- 7 (- scanline y)))))
		  ;; else, sprites are 16x8
		  (if (not (logbitp 7 attribute))
		      ;; if sprite is not flipped vertically
		      (if (< (- scanline y) 8)
			  ;; top half tile
			  (setf addr-lo (logior (ash (logand sprite-id #x01) 12)
						(ash (logand sprite-id #xFE) 4)
						(logand (- scanline y) #x07)))
			  ;; bottom half tile
			  (setf addr-lo (logior (ash (logand sprite-id #x01) 12)
						(ash (1+ (logand sprite-id #xFE)) 4)
						(logand (- scanline y) #x07))))
		      ;; sprite is flipped vertically
		      (if (< (- scanline y) 8)
			  ;; top half tile
			  (setf addr-lo (logior (ash (logand sprite-id #x01) 12)
						(ash (1+ (logand sprite-id #xFE)) 4)
						(- 7 (logand (- scanline y)) #x07)))
			  ;; bottom half tile
			  (setf addr-lo (logior (ash (logand sprite-id #x01) 12)
						(ash (logand sprite-id #xFE) 4)
						(- 7 (logand (- scanline y)) #x07))))))
	      (setf addr-hi (+ addr-lo 8))
	      (setf bits-lo (read/ppu->ppu ppu addr-lo))
	      (setf bits-hi (read/ppu->ppu ppu addr-hi))
	      (when (logbitp 6 attribute) ;; when sprite is inverted
		;; flip the bits!
		(setf bits-lo (reverse-byte bits-lo))
		(setf bits-hi (reverse-byte bits-hi)))
	      (setf (aref sprite-shifter-pattern-hi i) bits-hi)
	      (setf (aref sprite-shifter-pattern-lo i) bits-lo))))))
    
    (when (between scanline 241 260)
      (when (and (eql scanline 241)
		 (eql cycle 1))
	(setf (vertical-blank status) 1)
	(when (bit-set (enable-nmi control))
	  (setf nmi-bit t))))
    
    (let ((bg-pixel 0)
	  (bg-palette 0)
	  (fg-pixel 0)
	  (fg-palette 0)
	  (fg-priority 0)
	  (final-pixel 0)
	  (final-palette 0))
      
      (when (bit-set (render-background mask))
	(let* ((bit-mux (ash #x8000 (- fine-x)))
	       (p0-pixel (if (> (logand pattern-lo bit-mux) 0) 1 0))
	       (p1-pixel (if (> (logand pattern-hi bit-mux) 0) 1 0))
	       (bg-pal0 (if (> (logand attrib-lo bit-mux) 0) 1 0))
	       (bg-pal1 (if (> (logand attrib-hi bit-mux) 0) 1 0)))
	  (setf bg-pixel (logior (ash p1-pixel 1) p0-pixel))
	  (setf bg-palette (logior (ash bg-pal1 1) bg-pal0))))
      
      (when (bit-set (render-sprites mask))
	(loop-for-element-in sprite-scanline
	  (when (eql (x element) 0)
	    (let ((fg-pixel-lo (ldb (byte 1 7) (aref sprite-shifter-pattern-lo i)))
		  (fg-pixel-hi (ldb (byte 1 7) (aref sprite-shifter-pattern-hi i))))
	      (setf fg-pixel (logior (ash fg-pixel-hi 1) fg-pixel-lo))
	      (setf fg-palette (+ #x04 (logand (attribute element) #x03)))
	      (setf fg-priority (invert-bit (ldb (byte 1 5) (attribute element))))
	      (when (not (eql fg-pixel 0))
		(when (eql i 0)
		  (setf sprite-zero-hit-being-rendered t))
		(return))))))
      (cond
	((and (eql bg-pixel 0)
	      (eql fg-pixel 0)) ;; if both 0, the pixel shall be 0
	 (progn (setf final-pixel 0)
		(setf final-palette 0)))
	((and (eql bg-pixel 0)
	      (> fg-pixel 0)) ;; if background 0 and foreground >0 , the pixel is foreground
 	 (progn (setf final-pixel fg-pixel)
		(setf final-palette fg-palette)))
	((and (> bg-pixel 0)
	      (eql fg-pixel 0)) ;; else, pixel is background
	 (progn (setf final-pixel bg-pixel)
		(setf final-palette bg-palette)))
	((and (> bg-pixel 0)
	      (> fg-pixel 0)) ;; if both active, check to see if foreground has priority or not
	 (if (bit-set fg-priority)
	     (progn (setf final-pixel fg-pixel)
		    (setf final-palette fg-palette))
	     (progn (setf final-pixel bg-pixel)
		    (setf final-palette bg-palette)))
	 (when (and sprite-zero-hit-being-rendered sprite-zero-hit-possible)
	   (when (and (bit-set (render-background mask))
		      (bit-set (render-sprites mask)))
	     (if (and (not (bit-set (render-background-left mask)))
		      (not (bit-set (render-sprites-left mask))))
		 (when (between cycle 9 257)
		   (setf (sprite-zero-hit status) 1))
		 (when (between cycle 1 258)
		   (setf (sprite-zero-hit status) 1)))))))
      (unless (null (ppu-renderer ppu)) 
	(set-pixel ppu (get-color-from-palette-ram ppu final-palette final-pixel) (1- cycle) scanline)))
    (incf cycle)
    
    (when (>= cycle 341)
      (setf cycle 0)
      (incf scanline)
      (when (>= scanline 261)
	(setf scanline -1)
	(setf frame-complete t)))))
