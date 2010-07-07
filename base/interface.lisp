(in-package :sb-math)

(defun float-sizeof (type)
  (with-float-type-choice type 4 8 8 16))

(defun array-sap (array)
  (if (sb-kernel:simple-array-p array) 
      (sb-sys:vector-sap (sb-ext:array-storage-vector array))
      (multiple-value-bind (displacement offset)
	  (array-displacement array)
	(sb-sys:sap+ (sb-sys:vector-sap (sb-ext:array-storage-vector displacement))
		     (* offset (float-sizeof (array-element-type array)))))))

(defun pmatrix-sap (pmatrix)
  (array-sap (pmatrix-storage-vector pmatrix)))

#|
(define-alien-type complex-single-float
    (struct complex-single-float
	    (real single-float)
	    (imag single-float)))

(define-alien-type complex-double-float
    (struct complex-double-float
	    (real double-float)
	    (imag double-float)))
|#
(defun with-float-type-choice
    (type single double complex-single complex-double)
  (cond ((eq type 'single-float) single)
	((eq type 'double-float) double)
	((equal type '(complex single-float)) complex-single)
	((equal type '(complex double-float)) complex-double)))

(defun float-type-choice-prefix (type)
  (cond ((eq type 'single-float) 's)
	((eq type 'double-float) 'd)
	((equal type '(complex single-float)) 'c)
	((equal type '(complex double-float)) 'z)))

(defun concat-as-strings (args)
  (apply #'concatenate 'string (mapcar #'string args)))

(defun with-function-choice (function type)
  (intern
   (string-upcase
    (concat-as-strings
     (list '% (float-type-choice-prefix type) function)))
   :sb-math))

;; system area pointer of a complex number

(defun object-sap (object)
  (sb-sys:int-sap (logand (sb-vm::get-lisp-obj-address object)
                          (lognot sb-vm::lowtag-mask))))

;; may be a fix is needed for x86_64,
;; if the header size differs there
(defun complex-sap (complex)
  (sb-sys:sap+ (object-sap complex)
	       (if (sb-kernel:complex-single-float-p complex)
		   4 8)))

(defun maybe-complex (number)
  (if (complexp number)
      (complex-sap number)
      number))

;; callbacks

(defmacro defcallback (name type func sap-ref-fn)
  `(sb-alien::define-alien-callback ,name ,type ((x ,type))
     (funcall ,func (funcall ,sap-ref-fn x 0))))

