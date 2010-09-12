(in-package :sb-math)

;; general matrices

(defun rank2-array-p (matrix)
  (= (array-rank matrix) 2))

(deftype general-matrix ()
  '(and array
        (satisfies rank2-array-p)))

(defun dim0 (array) (array-dimension array 0))
(defun dim1 (array) (array-dimension array 1))

(defun make-matrix (dimensions &rest keys &key (element-type *default-type*) &allow-other-keys)
  (apply #'make-array dimensions :element-type element-type keys))

(defun make-matrix-like (matrix)
  (make-array (array-dimensions matrix) :element-type (array-element-type matrix)))

;; general matrix iteration

(defmacro do-matrix ((matrix &rest subscripts) &body body)
  (let ((i (or (first subscripts) (gensym)))
	(j (or (second subscripts) (gensym))))
    `(dotimes (,i (dim0 ,matrix))
       (declare (type fixnum i))
       ,(if (second subscripts)
	    `(dotimes (,j (dim1 ,matrix))
	       (declare (type fixnum j))
	       ,@body)
	    `(progn
	       ,@body)))))

(declaim (inline make-array))
(defun row-bind (matrix row)
  (declare (type (simple-array single-float) matrix)
	   (type fixnum row)
	   (inline dim1)
	   (optimize speed (safety 0) (space 0)))
  (make-array (dim1 matrix)
	      :element-type 'single-float
	      :displaced-to matrix
	      :displaced-index-offset (* (dim1 matrix) row)))
(declaim (notinline make-array))

(defmacro do-rows ((matrix row) &body body)
  (let ((i (gensym)))
    `(dotimes (,i (dim0 ,matrix))
       (let ((,row (row-bind ,matrix ,i)))
	 ,@body))))

;; general mapping

(defun map-matrix (matrix func)
  (dotimes (i (array-total-size matrix))
    (setf (row-major-aref matrix i)
	  (funcall func (row-major-aref matrix i))))
  matrix)

(defun map-two-matrices (m1 m2 func)
  (assert (= (array-total-size m1) (array-total-size m2)) nil "Matrix sizes not equal")
  (dotimes (i (array-total-size m1))
    (setf (row-major-aref v1 i)
	  (funcall func (row-major-aref v1 i) (row-major-aref v2 i))))
  m1)

(defun make-random-matrix (dimensions &key
			   (element-type *default-type*)
			   (rng #'simple-rng))
  (map-matrix (make-matrix dimensions :element-type element-type) rng))

;; printing matrices

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
  (do-matrix (matrix i)
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

;; approximate equality

(defmethod ~= ((X array) (Y array) eps)
  (when (equal (array-dimensions X) (array-dimensions Y))
    (let ((val (ammax (m- (copy X) Y))))
      (if (complexp val)
	  (and (< (realpart val) eps)
	       (< (imagpart val) eps))
	  (< val eps)))))

