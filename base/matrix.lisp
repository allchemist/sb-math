(in-package :sb-math)

(export
 '(dim0 dim1 make-matrix make-matrix-like make-random-matrix do-matrix print-matrix coerce-matrix linspace))

;; get matrix dimensions

(declaim (ftype (function (array) fixnum) dim0 dim1))
(declaim (inline dim0 dim1))

(defun dim0 (array)
  (declare (optimize speed)
	   (sb-ext:muffle-conditions sb-ext:compiler-note))
  (the fixnum (array-dimension array 0)))

(defun dim1 (array) 
  (declare (optimize speed)
	   (sb-ext:muffle-conditions sb-ext:compiler-note))
  (the fixnum (array-dimension array 1)))

;; matrix creating

(defmacro make-matrix (dimensions &rest keys &key &allow-other-keys)
  (if (member :element-type keys)
      `(make-array ,dimensions
		   :element-type ,(second keys)
		   ,@(cddr keys))
      `(make-array ,dimensions
		   :element-type *default-type*
		   ,@keys)))

(defmacro make-matrix-like (matrix)
  `(make-array (array-dimensions ,matrix) :element-type (array-element-type ,matrix)))

(defun make-random-matrix (dimensions &key
			   (element-type *default-type*)
			   (rng 'simple-rng))
  (map-matrix (make-matrix dimensions :element-type element-type) rng))

;; matrix iteration

(defmacro make-matrix-iterator ()
  (let ((defs nil))
    (dolist (type '((s single-float)
		    (d double-float)
		    (c (complex single-float))
		    (z (complex double-float))))
      (let ((name (intern (string-upcase (concat-as-strings (first type) 'do-matrix)) :sb-math)))
	(push `(export ',name) defs)
	(push
	 `(defmacro ,name
	      ((matrix &rest subscripts) &body body)
	    (let ((i (first subscripts))
		  (j (or (second subscripts) (gensym))))
	      `(dotimes (,i (the fixnum (dim0 ,matrix))
			 (the (simple-array ,',(second type)) ,matrix))
		 (declare (type (simple-array ,',(second type)) ,matrix)
			  (type fixnum i)
			  (optimize speed))
		 ,(if (second subscripts)
		      `(dotimes (,j (dim1 ,matrix))
			 (declare (type fixnum ,j))
			 ,@body)
		      `(progn
			 ,@body)))))
	 defs))
      (push 
       `(defmacro do-matrix ((matrix &rest subscripts) &body body)
	  `(float-choice-funcall (array-element-type ,matrix) do-matrix nil
				 (,matrix ,@subscripts) ,@body))
       defs))
    `(progn ,@(nreverse defs))))

(make-matrix-iterator)

;; printing matrices

(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(defun make-control-string (val prec exp?)
  (if (complexp val)
      (concatenate 'string
		   (if (minusp (realpart val)) " ~," "  ~,")
		   (write-to-string prec)
		   (string (if exp? #\e #\f))
		   (if (minusp (imagpart val)) ":~," ": ~,")
		   (write-to-string prec)
		   (string (if exp? #\e #\f))
		   " ")
      (concatenate 'string
		   (if (minusp val) " ~," "  ~,")
		   (write-to-string prec)
		   (string (if exp? #\e #\f))
		   " ")))

(defun maybe-complex-arglist (val)
  (if (complexp val)
      (list (realpart val) (imagpart val))
      (list val)))

(defgeneric print-matrix (matrix &key dest prec exp?))

(defmethod print-matrix ((matrix vector) &key (dest t) (prec 3) (exp? nil))
  (dotimes (i (dim0 matrix))
    (let ((val (aref matrix i)))
      (apply #'format dest (make-control-string val prec exp?) (maybe-complex-arglist val))))
  (terpri dest))

(defmethod print-matrix ((matrix array) &key (dest t) (prec 3) (exp? nil))
  (dotimes (i (dim0 matrix))
    (when (not (zerop i)) (terpri dest))
    (dotimes (j (dim1 matrix))
      (let ((val (aref matrix i j)))
	(apply #'format dest (make-control-string val prec exp?) (maybe-complex-arglist val)))))
  (terpri dest))

(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))

;; approximate equality

(defmethod ~= ((X array) (Y array) eps)
  (when (equal (array-dimensions X) (array-dimensions Y))
    (let ((val (ammax (m- (copy X) Y))))
      (if (complexp val)
	  (and (< (realpart val) eps)
	       (< (imagpart val) eps))
	  (< val eps)))))

;; type coercing

(defun coerce-matrix (matrix type)
  (let ((out (make-matrix (array-dimensions matrix) :element-type type)))
    (dotimes (i (array-total-size matrix))
      (setf (row-major-aref out i)
	    (coerce (row-major-aref matrix i) type)))
    out))

(defun linspace (xmin xmax points)
  (let* ((vec (make-matrix points))
	 (type (array-element-type vec)))
    (loop for idx from 0 below points
	  for val from xmin to xmax by (/ (- xmax xmin) (1- points))
	  do (setf (aref vec idx) (coerce val type)))
    vec))
