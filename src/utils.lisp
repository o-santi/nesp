;; all the macro definitions
;; and function definitions that
;; should spam multiple files
;; i will also define some multi-purpose generic functions here
;; and their respective definitions will be given in each file

(deftype u1 () '(unsigned-byte 1))
(deftype u8 () '(unsigned-byte 8))
(deftype u16 () '(unsigned-byte 16))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric read-memory (obj addr from &key)
  (:documentation "Read a value at addr from from and returns to reader"))

(defgeneric write-memory (obj addr data at &key)
  (:documentation "Write value data at addr from obj"))

(defgeneric byte->reg (obj data)
  (:documentation "Dump byteword into register object"))

(defgeneric reg->byte (obj)
  (:documentation "Dump register into byteword"))

(defgeneric show-reg (obj)
  (:documentation "Show bit values inside register"))

(defgeneric clock (obj)
  (:documentation "Clocks an obj"))

(defsubst bytememory (size)
  (make-array size :element-type 'u8 :initial-element 0))

(defsubst between (first second third)
  (and (>= first second)
       (<= first third)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defsubst as-keyword (sym) (intern (string sym) :keyword))

(defsubst mklist (x) (if (listp x) x (list x)))

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

(defsubst bit-vector->integer (bit-vector)
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

(defun slot->defreg-slot (slot)
  (let ((name (first slot)))
    `(,name :initarg ,(as-keyword name) :accessor ,name :initform 0 :type (unsigned-byte ,@(cdr slot)))))

(defsubst power (n m)
  (reduce #'* (loop for x below n collect m)))

(defun slot->position-sum (slot position)
  (let ((name (first slot)))
    `(* (,name obj) ,(power position 2))))

(defmacro define-register (name slots)
  `(progn
    (defclass ,name ()
      ,(mapcar #'slot->defreg-slot slots))
    
    (defmethod byte->reg ((obj ,name) byteword)
      ,@(loop for slot in slots and position = 0 then (+ (cadr slot) position)
	      collect `(setf (,(car slot) obj) (ldb (byte ,(car (cdr slot)) ,position) byteword))))

    (defmethod reg->byte ((obj ,name))
      (reduce #'+ (list ,@(loop for slot in slots and position = 0 then (+ (cadr slot) position)
		     collect (slot->position-sum slot position)))))

    (defmethod show-reg ((obj ,name))
      ,@(loop for slot in slots
	      collect `(progn
			 (format t "~a : ~a" ,(string (car slot)) (,(car slot) obj))
			 (terpri))))))

(defmacro when-bit-set ((integer bits) &body body)
  `(when (and ,@(loop for bit in (mklist bits) collect `(logbitp ,bit ,integer)))
     ,@body))

(defmacro setf&& (object integer)
  `(setf ,object (logand ,object ,integer)))
;; TODO: change to once-only

(defmacro setf>> (object integer)
  `(setf ,object (ash ,object ,integer)))
;; TODO: change to once-only
