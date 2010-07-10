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

(defmacro do-matrix ((matrix &rest subscripts) &body body)
  (let ((i (or (first subscripts) (gensym)))
	(j (or (second subscripts) (gensym))))
    `(dotimes (,i (dim0 ,matrix))
       ,(if (second subscripts)
	    `(dotimes (,j (dim1 ,matrix))
	       ,@body)
	    `(progn
	       ,@body)))))

(defun row-bind (matrix row)
  (make-array (dim1 matrix)
	      :element-type (array-element-type matrix)
	      :displaced-to matrix
	      :displaced-index-offset (* (dim1 matrix) row)))

(defmacro do-rows ((matrix row) &body body)
  (let ((i (gensym)))
    `(dotimes (i (dim0 ,matrix))
       (setf ,row (row-bind ,matrix i))
       ,@body)))

(defun map-matrix (matrix func)
  (let ((vec (sb-ext:array-storage-vector matrix)))
    (do-matrix (vec i)
      (setf (aref vec i) (funcall func (aref vec i))))
  matrix))

(defun map-two-matrices (m1 m2 func)
  (let ((v1 (sb-ext:array-storage-vector m1))
	(v2 (sb-ext:array-storage-vector m2)))
    (do-matrix (v1 i)
      (setf (aref v1 i)
	    (funcall func (aref v1 i) (aref v2 i))))
    m1))

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

;; general matrices, assumed as typed

(defun square-matrix-p (matrix)
  (= (dim0 matrix) (dim1 matrix)))

(defmacro do-upper-triangle ((matrix &optional i j) &body body)
  (let ((m (or i (gensym)))
	(n (or j (gensym))))
    `(assert (square-matrix-p ,matrix) nil "Matrix should be square")
    `(dotimes (,m (dim0 ,matrix))
       (loop for ,n from ,m to (1- (dim1 ,matrix))
	     do
	  (progn
	    ,@body)))))

(defmacro do-lower-triangle ((matrix &optional i j) &body body)
  `(do-upper-triangle (,matrix ,j ,i)
     ,@body))

(defun mirror-upper-triangle (matrix)
  (do-upper-triangle (matrix i j)
    (setf (aref matrix j i)
	  (aref matrix i j)))
  matrix)

(defun mirror-lower-triangle (matrix)
  (do-upper-triangle (matrix i j)
    (setf (aref matrix i j)
	  (aref matrix j i)))
  matrix)

(defun mirror-upper-hermitian-triangle (matrix)
  (do-upper-triangle (matrix i j)
    (setf (aref matrix j i)
	  (conjugate (aref matrix i j))))
  matrix)

(defun mirror-lower-hermitian-triangle (matrix)
  (do-upper-triangle (matrix i j)
    (setf (aref matrix i j)
	  (conjugate (aref matrix j i))))
  matrix)

(defmacro do-diag ((matrix &optional i) &body body)
  (let ((n (or i (gensym))))
    `(dotimes (,n (min (dim0 ,matrix) (dim1 ,matrix)))
       ,@body)))

(defun diag (vec &key dest)
  (let ((dim (dim0 vec)))
    (if dest
	(assert (and (>= (dim0 dest) dim)
		     (>= (dim1 dest) dim))
		nil "Destination matrix is smaller than provided vector")
	(setf dest
	      (make-matrix (list dim dim)
			   :element-type (array-element-type vec))))
    (dotimes (i dim)
      (setf (aref dest i i)
	    (aref vec i)))
    dest))

;; approximate equality

(defmethod ~= ((X array) (Y array) eps)
  (when (equal (array-dimensions X) (array-dimensions Y))
    (let ((val (ammax (m- (copy X) Y))))
      (if (complexp val)
	  (and (< (realpart val) eps)
	       (< (imagpart val) eps))
	  (< val eps)))))


;; packed matrices

;; types

(defclass packed-matrix ()
  ((storage-vector
    :initarg :storage-vector
    :accessor pmatrix-storage-vector)
   (ldm
    :initarg :ldm
    :accessor ldm)))

(defclass diagonal-matrix (packed-matrix)
  ((ncols
    :initarg :ncols
    :accessor ncols)))

(defclass triangular-matrix (packed-matrix)
  ((upper?
    :initarg :upper?
    :accessor upper?)))

(defclass symmetric-matrix (packed-matrix)
  ((upper?
    :initarg :upper?
    :accessor upper?)))

(defclass hermitian-matrix (packed-matrix)
  ((upper?
    :initarg :upper?
    :accessor upper?)))

(defun pmatrix-element-type (pmatrix)
  (array-element-type (pmatrix-storage-vector pmatrix)))

;; creating

(defun make-pmatrix (dimensions &key
		     (element-type *default-type*)
		     (pack-type :general)
		     (upper? t))
  (let ((ldm (first dimensions)))
    (ecase pack-type
      (:general (make-matrix dimensions :element-type element-type))
      (:diagonal
	 (make-instance 'diagonal-matrix
			:storage-vector (make-matrix (apply #'min dimensions)
						     :element-type element-type)
		      :ldm ldm
		      :ncols (second dimensions)))
      (:triangular
	 (progn
	   (assert (apply #'= dimensions) nil "Triangular matrix should be square")
	   (make-instance 'triangular-matrix
			  :storage-vector (make-matrix (/ (* ldm (1+ ldm)) 2)
						       :element-type element-type)
			  :ldm ldm
			  :upper? upper?)))
      (:symmetric
	 (progn
	   (assert (apply #'= dimensions) nil "Symmetric matrix should be square")
	   (make-instance 'symmetric-matrix
			  :storage-vector (make-matrix (/ (* ldm (1+ ldm)) 2)
						       :element-type element-type)
			  :ldm ldm
			  :upper? upper?)))
      (:hermitian
	 (progn
	   (assert (apply #'= dimensions) nil "Hermitian matrix should be square")
	   (assert (subtypep element-type 'complex)
		   nil "Hermitian matrix should be complex")
	   (make-instance 'hermitian-matrix
			  :storage-vector (make-matrix (/ (* ldm (1+ ldm)) 2)
						       :element-type element-type)
			  :ldm ldm
			  :upper? upper?))))))

;; mapping

(defun map-pmatrix (pmatrix func)
  (map-matrix (pmatrix-storage-vector pmatrix) func)
  pmatrix)

(defun map-two-pmatrices (pm1 pm2 func)
  (map-two-matrices (pmatrix-storage-vector pm1)
		    (pmatrix-storage-vector pm2)
		    func)
  pm1)

(defun make-random-pmatrix (dimensions &key
			    (element-type *default-type*)
			    (pack-type :general)
			    (upper? t)
			    (rng #'simple-rng))
  (let ((random-pmatrix (map-pmatrix (make-pmatrix dimensions
						   :element-type element-type
						   :pack-type pack-type
						   :upper? upper?)
				     rng)))
    (when (eq pack-type :hermitian)
      (let ((dim (first dimensions))
	    (pos (1- (- (first dimensions))))
	    (storage-vector (pmatrix-storage-vector random-pmatrix)))
	(dotimes (i dim)
	  (setf pos (+ pos 1 (- dim i)))
	  (setf (aref storage-vector pos)
		(complex (realpart (aref storage-vector pos)))))))
    random-pmatrix))

(defgeneric unpack-pmatrix (pmatrix &optional destination))

(defmethod unpack-pmatrix ((pmatrix diagonal-matrix) &optional destination)
  (let ((dest (or destination (make-matrix (list (ldm pmatrix) (ncols pmatrix))
					   :element-type (pmatrix-element-type pmatrix))))
	(storage-vector (pmatrix-storage-vector pmatrix)))
    (do-diag (dest i)
      (setf (aref dest i i)
	    (aref storage-vector i)))
    dest))

(defun hlp-unpack-pmatrix (pmatrix destination)
  (let ((dest (or destination (make-matrix (list (ldm pmatrix) (ldm pmatrix))
					   :element-type (pmatrix-element-type pmatrix))))
	(storage-vector (pmatrix-storage-vector pmatrix)))
    (let ((k 0))
      (if (upper? pmatrix)
	  (do-upper-triangle (dest i j)
	    (setf (aref dest i j) (aref storage-vector k))
	    (incf k))
	  (do-lower-triangle (dest i j)
	    (setf (aref dest i j) (aref storage-vector k))
	    (incf k))))
    dest))

(defmethod unpack-pmatrix ((pmatrix triangular-matrix) &optional destination)
  (hlp-unpack-pmatrix pmatrix destination))
	   
(defmethod unpack-pmatrix ((pmatrix symmetric-matrix) &optional destination)
  (funcall (if (upper? pmatrix)
	       #'mirror-upper-triangle
	       #'mirror-lower-triangle)
	   (hlp-unpack-pmatrix pmatrix destination)))

(defmethod unpack-pmatrix ((pmatrix hermitian-matrix) &optional destination)
  (funcall (if (upper? pmatrix)
	       #'mirror-upper-hermitian-triangle
	       #'mirror-lower-hermitian-triangle)
	   (hlp-unpack-pmatrix pmatrix destination)))

;; printing

(defmethod print-matrix ((matrix packed-matrix) &key (dest t) (prec 3) (exp? nil))
  (print-matrix (unpack-pmatrix matrix) :dest dest :prec prec :exp? exp?))


;; approximate equality

(defmethod ~= ((X packed-matrix) (Y packed-matrix) eps)
  (and (eq (class-of X) (class-of Y)))
  (~= (pmatrix-storage-vector X) (pmatrix-storage-vector Y) eps))
