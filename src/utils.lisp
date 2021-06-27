;; all the macro definitions
;; and function definitions that
;; should spam multiple files
;; i will also define some multi-purpose generic functions here
;; and their respective definitions will be given in each file

(in-package #:nes)

(deftype u1 () '(unsigned-byte 1))
(deftype u8 () '(unsigned-byte 8))
(deftype u16 () '(unsigned-byte 16))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric reset-component (obj)
  (:documentation "Resets an obj"))

(defun bytememory (size)
  (make-array size :element-type 'u8 :initial-element 0))

(declaim (inline between))
(defun between (first second third)
  (and (>= first second)
       (<= first third)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

;; this monster was taken from http://www.gigamonkeys.com/book/macros-defining-your-own.html

(declaim (inline between))
(defun as-keyword (sym) (intern (string sym) :keyword))

(declaim (inline mklist))
(defun mklist (x) (if (listp x) x (list x)))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defmacro define-binary-class (name slots)
  (with-gensyms (typevar objectvar streamvar)
    `(progn
       (defclass ,name ()
         ,(mapcar #'slot->defclass-slot slots))

       (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
         (let ((,objectvar (make-instance ',name)))
           (with-slots ,(mapcar #'first slots) ,objectvar
             ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
           ,objectvar)))))

(defmethod read-value ((type (eql 'u8)) in &key length)
  (if (null length)
      (read-byte in)
      (let ((memory (bytememory length)))
	(dotimes (i length)
	  (setf (aref memory i) (read-byte in)))
	memory)))
  
(defmethod read-value ((type (eql 'string)) in &key length)
  (let ((string (make-string length)))
      (dotimes (i length)
        (setf (char string i) (code-char (read-byte in))))
      string))

(defun slot->bit-accessor (classname slot position)
  (let ((name (car slot))
	(size (cadr slot)))
    `(progn
       (declaim (ftype (function (,classname) (unsigned-byte ,size)) ,name)
		(inline ,name))
       (defun ,name (obj)
	 (declare (optimize (speed 3) (safety 0))
		  (values (unsigned-byte ,size)))
	 (ldb (byte ,size ,position) (reg obj)))

       (declaim (ftype (function (,classname u8))) (setf ,name)
		(inline ,name))
       (defun (setf ,name) (data obj)
	 (declare (optimize (speed 3) (safety 0)))
	 (setf (ldb (byte ,size ,position) (reg obj)) data)))))

(defmacro define-register (name slots)
  (let ((number-of-bits (loop for slot in slots sum (cadr slot)))
	(slot-names (loop for slot in slots collect (car slot))))
    `(progn
       (defclass ,name ()
	 ((register :initform 0 :accessor reg :type (unsigned-byte ,number-of-bits) :initarg :reg)))
       
       ,@(loop for slot in slots and position = 0 then (+ (cadr slot) position)
	       collect (slot->bit-accessor name slot position))
	 
       (defmethod print-object ((obj ,name) out)
	 (print-unreadable-object (obj out :type t)
	   (format out "~{~{~a: #~X ~}~}" (loop for slot-name in ',slot-names
						collect (list (string slot-name) (funcall slot-name obj)))))))))
	 
(declaim (inline bit-set))
(defun bit-set (integer &key (pos 0))
  (declare (type fixnum integer))
  (logbitp pos integer))

(defmacro setf&& (object integer)
  `(setf ,object (logand ,object ,integer)))
;; TODO: change to once-only

(defmacro setf>> (object integer)
  `(setf ,object (ash ,object ,integer)))
;; TODO: change to once-only

(defmacro with-register-bits (bit-names reg-obj &body body)
  `(symbol-macrolet ,(mapcar #'(lambda (x) `(,x (,x ,reg-obj))) bit-names)
     ,@body))

(defmacro loop-for-element-in (array &body body)
  `(dotimes (i (length ,array))
     (declare (type (integer 0 *) i))
     (symbol-macrolet ((element (aref ,array i)))
       ,@body)))

(defun slot->def-component-slot (slot)
  (let* ((spec (mklist slot))
	 (name (first spec))
	 (initform (second spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name :initform ,initform)))

(declaim (ftype (function (u8) u8) reverse-byte))
(defun reverse-byte (number)
  (declare (type u8 number)
	   (values u8))
  (setf number (logior (ash (logand number #xF0) -4) (ash (logand number #x0F) 4))) ;; inverts 4 first numbers
  (setf number (logior (ash (logand number #xCC) -2) (ash (logand number #x33) 2))) ;; inverts every 2 number pair
  (setf number (logior (ash (logand number #xAA) -1) (ash (logand number #x55) 1))) ;; inverts all single bits
  number)

(declaim (inline invert-bit))
(defun invert-bit (bit)
  (declare (type bit bit))
  (logxor bit 1))
	   

