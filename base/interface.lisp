(in-package :sb-math2)

(declaim (inline float-sizeof array-sap object-sap complex-sap maybe-complex))

(declaim (ftype (function (t) integer) float-sizeof))
(defun float-sizeof (type)
  (the integer
    (cond ((eq type 'single-float) 4)
	  ((eq type 'double-float) 8)
	  ((equal type '(complex single-float)) 8)
	  ((equal type '(complex double-float)) 16)
	  (t (error "Not a float type: ~A" type)))))

;; pointer access

(declaim (ftype (function (simple-array) system-area-pointer) array-sap))
(defun array-sap (array)
  (declare (type simple-array array)
	   (optimize speed)
	   (sb-ext:muffle-conditions sb-ext:compiler-note))
  (the system-area-pointer
    (sb-sys:vector-sap
     (the simple-array
       (sb-ext:array-storage-vector array)))))


#|
this gets sap of displaced array, too
(defun array-sap (array)
  (declare (type array array)
	   (optimize speed)
	   (sb-ext:muffle-conditions sb-ext:compiler-note))
  (if (not (array-displacement array))
      (the system-area-pointer
	(sb-sys:vector-sap
	 (the simple-array
	   (sb-ext:array-storage-vector array))))
      (multiple-value-bind (displacement offset)
	  (array-displacement array)
	(the system-area-pointer
	  (sb-sys:sap+ (the system-area-pointer
			 (sb-sys:vector-sap
			  (the simple-array
			    (sb-ext:array-storage-vector (the simple-array displacement)))))
		       (the integer
			 (* (the integer offset)
			    (the integer (float-sizeof (array-element-type array))))))))))|#



;; system area pointer of a complex number

(defun object-sap (object)
  (sb-sys:int-sap (logand (sb-vm::get-lisp-obj-address object)
                          (lognot sb-vm::lowtag-mask))))

;; may be a fix is needed for x86_64,
;; if the header size differs there
(defun complex-sap (complex)
  (declare (optimize speed)
	   (inline object-sap)
	   (sb-ext:muffle-conditions sb-ext:compiler-note))
  (the system-area-pointer
    (sb-sys:sap+ (the system-area-pointer (object-sap complex))
		 (the integer (if (sb-kernel:complex-single-float-p complex)
		   4 8)))))

(defun maybe-complex (number)
  (if (complexp number)
      (complex-sap number)
      number))

;; callbacks

(defmacro function-sap (func type)
  `(alien-sap
    (sb-alien::alien-lambda ,type ((x ,type)) ,(cdr `(,@func x)))))
