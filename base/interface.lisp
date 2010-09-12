(in-package :sb-math)

(declaim (inline array-sap maybe-complex float-sizeof))

;; type defining

(defun concat-as-strings (&rest args)
  (apply #'concatenate 'string (mapcar #'string (remove nil args))))

(defmacro float-type-choice (type single double complex-single complex-double)
  `(cond ((eq ,type 'single-float) ,single)
	 ((eq ,type 'double-float) ,double)
	 ((equal ,type '(complex single-float)) ,complex-single)
	 ((equal ,type '(complex double-float)) ,complex-double)))

(defmacro float-function-choice (function type &key no-%)
  `(cond ((eq ,type 'single-float)
	  ',(intern (string-upcase (concat-as-strings (if no-% 's '%s) function)) :sb-math))
	 ((eq ,type 'double-float)
	  ',(intern (string-upcase (concat-as-strings (if no-% 'd '%d) function)) :sb-math))
	 ((equal ,type '(complex single-float))
	  ',(intern (string-upcase (concat-as-strings (if no-% 'c '%c) function)) :sb-math))
	 ((equal ,type '(complex double-float))
	  ',(intern (string-upcase (concat-as-strings (if no-% 'z '%z) function)) :sb-math))))


(defmacro float-choice-funcall (type func prefix &rest args)
  `(float-type-choice
    ,type
    (,(intern (string-upcase (concat-as-strings prefix 's func)))
      ,@args)
    (,(intern (string-upcase (concat-as-strings prefix 'd func)))
      ,@args)
    (,(intern (string-upcase (concat-as-strings prefix 'c func)))
      ,@args)
    (,(intern (string-upcase (concat-as-strings prefix 'z func)))
      ,@args)))

(defun float-sizeof (type)
  (cond ((eq type 'single-float) 4)
	((eq type 'double-float) 8)
	((equal type '(complex single-float)) 8)
	((equal type '(complex double-float)) 16)
	(t (error "Not a float type: ~A" type))))

;; pointer access

(defun array-sap (array)
  (if (not (array-displacement array))
      (sb-sys:vector-sap (sb-ext:array-storage-vector array))
      (multiple-value-bind (displacement offset)
	  (array-displacement array)
	(sb-sys:sap+ (sb-sys:vector-sap (sb-ext:array-storage-vector displacement))
		     (* offset (float-sizeof (array-element-type array)))))))

(defun pmatrix-sap (pmatrix)
  (declare (inline pmatrix-storage-vector))
  (array-sap (pmatrix-storage-vector pmatrix)))

;; system area pointer of a complex number

(defun object-sap (object)
  (sb-sys:int-sap (logand (sb-vm::get-lisp-obj-address object)
                          (lognot sb-vm::lowtag-mask))))

;; may be a fix is needed for x86_64,
;; if the header size differs there
(defun complex-sap (complex)
  (declare (inline object-sap))
  (sb-sys:sap+ (object-sap complex)
	       (if (sb-kernel:complex-single-float-p complex)
		   4 8)))

(defun maybe-complex (number)
  (declare (inline complex-sap))
  (if (complexp number)
      (complex-sap number)
      number))

;; callbacks

(defmacro function-sap (func type)
  `(alien-sap
    (sb-alien::alien-lambda ,type ((x ,type)) ,(cdr `(,@func x)))))
