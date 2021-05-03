(in-package :cl-user)

(defpackage :binary-parser
  (:use :common-lisp)
  (:export :cartridge
           :with-gensyms
	   :define-binary-class
	   :read-value
	   :cartridge-header
	   :parse-cartridge))

(deftype u8 () '(unsigned-byte 8))

(defun bytememory (size)
  (make-array size :element-type 'u8 :initial-element 0))

(defstruct cartridge
  filename
  (mapper-id 0 :type u8)
  (prog-banks 0 :type u8)
  (char-banks 0 :type u8) 
  (v-prog-mem (make-array 0 :fill-pointer 0 :adjustable t))
  (v-char-mem (make-array 0 :fill-pointer 0 :adjustable t)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun mklist (x) (if (listp x) x (list x)))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defun bit-vector->integer (bit-vector)
  "Create a positive integer from a bit-vector."
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

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

(defmethod read-value ((type (eql 'u8)) in &key (length 1))
  (let ((memory (bytememory length)))
    (dotimes (i length)
      (setf (aref memory i) (read-byte in)))
    memory))

(defmethod read-value ((type (eql 'string)) in &key length)
  (let ((string (make-string length)))
      (dotimes (i length)
        (setf (char string i) (code-char (read-byte in))))
      string))

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


(defun parse-cartridge (filename)
  (with-open-file (filestream filename :element-type 'u8)
    (let* ((header (read-value 'cartridge-header filestream))
	   ;(mapper-id (get-mapper-id header))
	   (cartridge (make-cartridge))
	   (prg-mem (* 16384 (bit-vector->integer (pgr-rom-size header))))
	   (chr-mem (* 8192 (bit-vector->integer (chr-rom-size header)))))
      
      (read-value 'u8 filestream :length 512) ;; just junk, does not matter
      ;; adjust the size of the virtual program memory to the cartridge struct
      (adjust-array (cartridge-v-prog-mem cartridge) prg-mem)
      ;;read into it
      (setf (cartridge-v-prog-mem cartridge) (read-value 'u8 filestream :length prg-mem))
      ;; adjust the size of the character program memory
      (adjust-array (cartridge-v-prog-mem cartridge) chr-mem)
      ;; read into it
      (setf (cartridge-v-prog-mem cartridge) (read-value 'u8 filestream :length chr-mem))
    cartridge)))

